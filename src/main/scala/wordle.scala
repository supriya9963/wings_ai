// file: wordle.scala
import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Random
import scala.collection.mutable.{Map as MutableMap}
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.client.RequestBuilding.Post
import org.apache.pekko.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse}
import org.apache.pekko.actor.{ActorSystem, ClassicActorSystemProvider}
import io.circe.parser.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}

object WordleSolver:

  given system: ActorSystem = ActorSystem("WordleSolverSystem")
  given ClassicActorSystemProvider = system

  case class RegisterRequest(mode: String, name: String) derives Encoder.AsObject
  case class RegisterResponse(id: String) derives Decoder, Encoder.AsObject
  case class CreateGameRequest(id: String, overwrite: Boolean) derives Encoder.AsObject
  case class GuessRequest(guess: String, id: String) derives Encoder.AsObject
  case class GuessResponse(feedback: Option[String], message: Option[String]) derives Decoder

  val instructions = """For every guessed word, the server provides feedback.
                       |   g = Green (correct letter, correct position)
                       |   y = Yellow (correct letter, wrong position)
                       |   r = Red (letter not in the word)""".stripMargin

  def registerPlayer(playerName: String): Try[String] =
    val requestBody = RegisterRequest("wordle", playerName).asJson.noSpaces
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = "https://wordle.we4shakthi.in/game/register",
      entity = HttpEntity(ContentTypes.`application/json`, requestBody)
    )

    Try(Await.result(Http().singleRequest(request), 10.seconds))
      .flatMap { response =>
        response.status match
          case status if status.isSuccess =>
            Try(Await.result(response.entity.toStrict(10.seconds), 10.seconds))
              .flatMap { strictEntity =>
                decode[RegisterResponse](strictEntity.data.utf8String).toTry
              }.map(_.id)
          case status =>
            Failure(new RuntimeException(s"Failed to register player: HTTP ${status.intValue}"))
      }

  def main(args: Array[String]): Unit =
    val playerName = "supriya"
    val allWords = Try(Source.fromFile("5words.txt").getLines().filter(_.length == 5).toVector).toOption

    allWords match
      case None =>
        println("Error: 5words.txt not found.")
        summon[ActorSystem].terminate()
      case Some(words) =>
        registerPlayer(playerName) match
          case Success(playerId) =>
            println(s"Registered player '$playerName' with ID: $playerId")
            val solver = new WordleSolver(playerId, words)
            solver.playUntilWin()
          case Failure(e) =>
            println(s"Error registering player: $e")
            summon[ActorSystem].terminate()


class WordleSolver(playerId: String, allWords: Vector[String]):
  // Since this is in the same file, we can directly access the companion object members
  // without a separate import statement.
  import WordleSolver.given_ClassicActorSystemProvider
  import io.circe.syntax.*

  private var availableWords = allWords
  private var attemptNum = 0
  private var currentGuess = ""

  // Use a secret word to simulate the game offline
  private val secretWord = "ulnad"

  private def startGame(): Unit =
    val requestBody = WordleSolver.CreateGameRequest(playerId, overwrite = true).asJson.noSpaces
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = "https://wordle.we4shakthi.in/game/create",
      entity = HttpEntity(ContentTypes.`application/json`, requestBody)
    )
    
    Try(Await.result(Http().singleRequest(request), 10.seconds)) match
      case Success(response) =>
        response.status match
          case status if status.isSuccess =>
            println("🔹 New game started.")
          case status =>
            println(s"Game creation failed: HTTP ${status.intValue}")
      case Failure(e) =>
        println(s"Game creation failed: $e")

    // Reset word list for the new game
    availableWords = allWords
    attemptNum = 0

  // New function to simulate feedback based on a secret word
  private def getSimulatedFeedback(guess: String, secret: String): String =
    val feedback = Array.fill(5)('r')
    val secretLetters = secret.toBuffer

    // First pass: find greens
    for i <- 0 until 5 do
      if guess(i) == secret(i) then
        feedback(i) = 'g'
        secretLetters.remove(secretLetters.indexOf(guess(i)))

    // Second pass: find yellows
    for i <- 0 until 5 do
      if feedback(i) == 'r' && secretLetters.contains(guess(i)) then
        feedback(i) = 'y'
        secretLetters.remove(secretLetters.indexOf(guess(i)))

    feedback.mkString

  private def filterWords(feedback: String): Unit =
    def dropBlacks(blacks: String, word: String): Boolean = blacks.forall(b => !word.contains(b))
    def pickGreens(greens: Vector[Char], word: String): Boolean = (0 until 5).forall(i => greens(i) == ' ' || word(i) == greens(i))
    def pickAmbers(ambers: MutableMap[Char, Vector[Int]], word: String): Boolean =
      ambers.forall { case (ch, badPositions) => word.contains(ch) && !badPositions.exists(pos => word(pos) == ch) }

    val greens = Vector.fill(5)(' ').toBuffer
    val blacks = new StringBuilder
    val ambers = MutableMap.empty[Char, Vector[Int]]

    for i <- 0 until 5 do
      val feedbackChar = feedback(i).toLower
      val guessChar = currentGuess(i)
      feedbackChar match
        case 'g' => greens(i) = guessChar
        case 'y' => ambers.updateWith(guessChar)(existing => Some(existing.getOrElse(Vector.empty) :+ i))
        case 'r' => blacks.append(guessChar)
        case _ => // Do nothing for invalid feedback characters

    availableWords = availableWords.filter { word =>
      dropBlacks(blacks.toString, word) && pickGreens(greens.toVector, word) && pickAmbers(ambers, word)
    }

  def playUntilWin(): Unit =
    println(WordleSolver.instructions)
    println(s"📝 Starting a new simulated game with secret word: $secretWord")

    // No need to call startGame() if we're simulating feedback
    availableWords = allWords
    attemptNum = 0

    while true do
      if availableWords.isEmpty then
        println("⚠️ Ran out of candidate words — something is wrong. Restarting simulation.")
        availableWords = allWords
        attemptNum = 0
        if availableWords.isEmpty then
          println("Error: The initial word list is empty. Cannot continue.")
          return

      val shuffledWords = Random.shuffle(availableWords)
      currentGuess = shuffledWords.head
      availableWords = shuffledWords.tail
      attemptNum += 1
      println(s"\nAttempt $attemptNum: Is it '$currentGuess'?")

      // Simulate the feedback instead of calling the server
      val feedback = getSimulatedFeedback(currentGuess, secretWord)
      println(s"Feedback: $feedback")

      if feedback.toLowerCase.forall(_ == 'g') then
        println(s"\n🎉 The computer guessed the correct word '$currentGuess' in $attemptNum attempts!")
        return // Exit the loop on win
      else
        filterWords(feedback)
