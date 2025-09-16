{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither)
import Control.Exception (catch, SomeException)

type SessionId = T.Text

data GameState = GameState
    { session :: SessionId
    , fullWords :: [T.Text]
    , remainingWords :: [T.Text]
    , attempts :: Int
    }

-- Load word list
loadWords :: FilePath -> IO [T.Text]
loadWords path = do
    content <- TIO.readFile path
    return $ filter (\w -> T.length w == 5) $ T.lines content

-- Register the player and return the cookie and session ID
registerPlayer :: Manager -> T.Text -> IO (CookieJar, SessionId)
registerPlayer manager name = do
    let payload = object ["mode" .= ("wordle" :: T.Text), "name" .= name]
    initialRequest <- parseRequest "https://wordle.we4shakthi.in/game/register"
    let request = initialRequest
                    { method = "POST"
                    , requestHeaders = [("Content-Type", "application/json")]
                    , requestBody = RequestBodyLBS $ encode payload
                    }
    response <- httpLbs request manager
    let body = responseBody response
    let jar = responseCookieJar response
    putStrLn $ "Registration response: " ++ LBS.unpack body
    case eitherDecode body of
        Right val -> case parseMaybe (.: "id") val of
            Just sid -> return (jar, sid)
            Nothing -> error "Failed to parse session ID from response"
        Left err -> error $ "Failed to decode JSON: " ++ err

-- Create a game with the session ID
createGame :: Manager -> CookieJar -> SessionId -> IO CookieJar
createGame manager jar sid = do
    let payload = object ["id" .= sid, "overwrite" .= True]
    initialRequest <- parseRequest "https://wordle.we4shakthi.in/game/create"
    let request = initialRequest
                    { method = "POST"
                    , requestHeaders = [("Content-Type", "application/json")]
                    , requestBody = RequestBodyLBS $ encode payload
                    , cookieJar = Just jar
                    }
    response <- httpLbs request manager
    let body = responseBody response
    let newJar = responseCookieJar response
    putStrLn $ "Game creation response: " ++ LBS.unpack body
    putStrLn "Game created."
    return newJar

-- Parse the feedback - API returns "RRRRY" format based on the example
parseFeedback :: T.Text -> [Char]
parseFeedback = T.unpack

-- Improved word filtering based on feedback
filterWords :: [Char] -> T.Text -> [T.Text] -> [T.Text]
filterWords feedback guess remainingWords = filter isValid remainingWords
  where
    guessChars = T.unpack guess
    isValid word = and $ zipWith3 (checkPosition word) guessChars feedback [0..]
    
    checkPosition word guessChar feedbackChar pos = case feedbackChar of
        'G' -> T.index word pos == guessChar  -- Correct position
        'Y' -> guessChar `T.elem` word && T.index word pos /= guessChar  -- Wrong position but in word
        'R' -> not (guessChar `T.elem` word)  -- Not in word at all
        _   -> True  -- Unknown feedback, keep word

-- Choose best word using simple frequency strategy
chooseBestWord :: [T.Text] -> IO T.Text
chooseBestWord [] = error "No words available to choose from"
chooseBestWord words = do
    idx <- randomRIO (0, length words - 1)
    return $ words !! idx

-- Play game with better error handling
playGame :: Manager -> CookieJar -> GameState -> IO ()
playGame manager jar gs@(GameState sid full remain att)
    | null remain = do
        putStrLn "No more words to try. Game over."
        return ()
    | otherwise = do
        guess <- chooseBestWord remain
        putStrLn $ "Attempt " ++ show (att + 1) ++ ": " ++ map toUpper (T.unpack guess)
        
        -- Send guess with error handling
        result <- catch (sendGuess manager jar sid guess) handleError
        case result of
            Nothing -> do
                -- Try next word without incrementing attempt count
                let newRemaining = filter (/= guess) remain
                playGame manager jar $ GameState sid full newRemaining att
            Just feedback -> do
                putStrLn $ "Feedback: " ++ T.unpack feedback
                
                -- Check if we won (all letters are correct)
                if T.all (== 'G') feedback
                    then putStrLn $ "The computer guessed the correct word: " ++ map toUpper (T.unpack guess) ++ "!"
                    else do
                        let newRemaining = filterWords (parseFeedback feedback) guess remain
                        playGame manager jar $ GameState sid full newRemaining (att + 1)
  where
    handleError :: SomeException -> IO (Maybe T.Text)
    handleError e = do
        putStrLn $ "Network error: " ++ show e
        return Nothing
    
    toUpper :: Char -> Char
    toUpper c
        | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
        | otherwise = c

-- Separate function to send guess
sendGuess :: Manager -> CookieJar -> SessionId -> T.Text -> IO (Maybe T.Text)
sendGuess manager jar sid guess = do
    let payload = object ["guess" .= guess, "id" .= sid]
    initialRequest <- parseRequest "https://wordle.we4shakthi.in/game/guess"
    let request = initialRequest
                    { method = "POST"
                    , requestHeaders = [("Content-Type", "application/json")]
                    , requestBody = RequestBodyLBS $ encode payload
                    , cookieJar = Just jar
                    }
    response <- httpLbs request manager
    let body = responseBody response
    
    case eitherDecode body of
        Left err -> return Nothing
        Right val -> do
            -- Try different possible field names for feedback
            let feedback = case parseMaybe (.: "feedback") val of
                    Just (String fb) -> fb
                    Just Null -> ""
                    Nothing -> case parseMaybe (.: "result") val of
                        Just (String fb) -> fb
                        Nothing -> case parseMaybe (.: "response") val of
                            Just (String fb) -> fb
                            Nothing -> ""
            
            if T.null feedback
                then return Nothing
                else return $ Just feedback
main :: IO ()
main = do
    putStrLn "Loading word list..."
    wordList <- loadWords "5words.txt" `catch` (\e -> do
        putStrLn $ "Error loading word list: " ++ show (e :: SomeException)
        return []
        )

    putStrLn "Starting Wordle computer guesser..."
    if null wordList
       then putStrLn "No words loaded. Please check if '5words.txt' exists and contains 5-letter words."
       else do
           manager <- newManager tlsManagerSettings

           catch (do
               -- Registration
               (jar, sid) <- registerPlayer manager "supriya"
               -- Game creation
               updatedJar <- createGame manager jar sid
               -- Automated game play
               playGame manager updatedJar $ GameState sid wordList wordList 0
               ) (\e -> putStrLn $ "Game error: " ++ show (e :: SomeException))
             
