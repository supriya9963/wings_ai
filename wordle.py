import requests
import random

class WordleSolver:
    """A class to manage the logic of solving a Wordle game with unlimited guesses."""
    instructions = """For every guessed word, the server provides feedback.
         g = Green (correct letter, correct position)
         y = Yellow (correct letter, wrong position)
         r = Red (letter not in the word)"""

    def __init__(self, session, player_id, all_words):
        self.session = session
        self.player_id = player_id
        self.all_words = all_words[:]
        self.available_words = all_words[:]
        self.attempt_num = 0
        self.status = "PLAY"
        self.guess = ""

    def _start_game(self):
        """Creates a new game on the server."""
        response = self.session.post(
            "https://wordle.we4shakthi.in/game/create", 
            json={"id": self.player_id, "overwrite": True}
        )
        if response.status_code in (200, 201):
            print("🔹 New game started.")
        else:
            print(f"Game creation failed: HTTP {response.status_code}")

        # Reset word list for the new game
        self.available_words = self.all_words[:]
        self.attempt_num = 0
        self.status = "PLAY"

    def _filter_words(self, feedback: str):
        """Filters the word list based on the server's feedback."""
        def drop_blacks(blacks: str, word: str) -> bool: 
            return all(b not in word for b in blacks)

        def pick_greens(greens: list, word: str) -> bool:
            for i in range(5):
                if greens[i] != " " and word[i] != greens[i]:
                    return False
            return True

        def pick_ambers(ambers: dict, word: str) -> bool:
            for ch, bad_pos in ambers.items():
                if ch not in word:
                    return False
                if any(word[pos] == ch for pos in bad_pos):
                    return False  
            return True

        greens = [" ", " ", " ", " ", " "]
        blacks = ""
        ambers = {}

        for i in range(5):
            letter = feedback[i].lower()
            guess_char = self.guess[i]
            if letter == "g":
                greens[i] = guess_char
            elif letter == "y":
                if guess_char not in ambers:
                    ambers[guess_char] = []
                ambers[guess_char].append(i)
            elif letter == "r":
                blacks += guess_char

        self.available_words = [
            word for word in self.available_words
            if drop_blacks(blacks, word)
            and pick_greens(greens, word)
            and pick_ambers(ambers, word)
        ]

    def play_until_win(self):
        """Keeps playing across multiple games until the correct word is guessed."""
        print(self.instructions)
        self._start_game()

        while True:
            if not self.available_words:
                print("⚠️ Ran out of candidate words — restarting game.")
                self._start_game()
                continue

            random.shuffle(self.available_words)
            self.guess = self.available_words.pop(0)
            self.attempt_num += 1
            print(f"\nAttempt {self.attempt_num}: Is it '{self.guess}'?")

            response = self.session.post(
                "https://wordle.we4shakthi.in/game/guess", 
                json={"guess": self.guess, "id": self.player_id}
            )

            try:
                result = response.json()
            except requests.exceptions.JSONDecodeError:
                print(f"❌ Error decoding JSON: {response.text}")
                continue

            feedback = result.get('feedback')
            message = result.get('message', 'No message')
            print(f"Message: {message}")

            # Restart game if server ended it or guess limit exceeded
            if "exceeded" in message.lower() or "no game" in message.lower():
                print("🔁 Restarting a new game due to server limit.")
                self._start_game()
                continue

            if not feedback or len(feedback) != 5:
                print("⚠️ Invalid feedback, skipping...")
                continue

            print(f"Feedback: {feedback}")
            if all(c.lower() == "g" for c in feedback):
                print(f"\n🎉 The computer guessed the correct word '{self.guess}' in {self.attempt_num} attempts!")
                self.status = "WON"
                break

            self._filter_words(feedback.lower())


def main():
    """Main function to set up and run the Wordle bot."""
    player_name = "supriya"
    
    try:
        with open("5words.txt", "r") as file:
            all_words = [line.strip() for line in file if len(line.strip()) == 5]
    except FileNotFoundError:
        print("Error: 5words.txt not found.")
        return

    with requests.Session() as session:
        # Register player
        try:
            response = session.post(
                "https://wordle.we4shakthi.in/game/register", 
                json={"mode": "wordle", "name": player_name}
            )
            response.raise_for_status()
            player_id = response.json().get("id")
            if not player_id:
                print("Could not get player ID from server.")
                return
        except requests.exceptions.RequestException as e:
            print(f"Error registering player: {e}")
            return

        print(f"Registered player '{player_name}' with ID: {player_id}")

        # Create solver and start unlimited guessing
        game = WordleSolver(session, player_id, all_words)
        game.play_until_win()


if __name__ == "__main__":
    main()

