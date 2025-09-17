# wordle_solver.ex
# A simple Wordle client in Elixir:
# - Register player
# - Create game
# - Make guesses and print feedback

Mix.install([
  {:httpoison, "~> 1.8"},
  {:jason, "~> 1.4"}
])

defmodule WordleSolver do
  alias HTTPoison.Response

  # ✅ Use hosted server instead of localhost
  @base_url "https://wordle.we4shakthi.in/game"
  @register_url "#{@base_url}/register"
  @create_url "#{@base_url}/create"
  @guess_url "#{@base_url}/guess"

  # Register player and capture cookie
  def register_player(name) do
    payload = %{"mode" => "wordle", "name" => name} |> Jason.encode!()

    case HTTPoison.post(@register_url, payload, [{"Content-Type", "application/json"}]) do
      {:ok, %Response{status_code: code, body: body, headers: headers}} when code in [200, 201] ->
        case Jason.decode(body) do
          {:ok, %{"id" => sid}} ->
            cookies =
              headers
              |> Enum.filter(fn {k, _} -> String.downcase(k) == "set-cookie" end)
              |> Enum.map(fn {_, v} -> String.split(v, ";") |> List.first() end)

            IO.puts("✅ Registered player #{name} with session #{sid}")
            {:ok, sid, cookies}

          _ ->
            {:error, "Failed to parse session ID"}
        end

      {:ok, %Response{status_code: code, body: body}} ->
        {:error, "Registration failed: HTTP #{code} - #{body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end

  # Create game using session id and cookie
  def create_game(session_id, cookies) do
    payload = %{"id" => session_id, "overwrite" => true} |> Jason.encode!()

    headers = [
      {"Content-Type", "application/json"},
      {"cookie", Enum.join(cookies, "; ")}
    ]

    case HTTPoison.post(@create_url, payload, headers) do
      {:ok, %Response{status_code: code, body: _body}} when code in [200, 201] ->
        IO.puts("✅ Game created successfully")
        {:ok, cookies}

      {:ok, %Response{status_code: code, body: body}} ->
        {:error, "Failed to create game: HTTP #{code} - #{body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end

  # Make a guess (include id + guess in payload)
  def make_guess(session_id, word, cookies) do
    payload = Jason.encode!(%{"id" => session_id, "guess" => word})

    headers = [
      {"Content-Type", "application/json"},
      {"cookie", Enum.join(cookies, "; ")}
    ]

    case HTTPoison.post(@guess_url, payload, headers) do
      {:ok, %Response{status_code: code, body: body}} when code in [200, 201] ->
        case Jason.decode(body) do
          {:ok, %{"feedback" => fb, "message" => msg}} ->
            IO.puts("➡️  Guess: #{word} | Feedback: #{fb} | Msg: #{msg}")
            {:ok, fb, msg}

          _ ->
            {:error, "Bad response: #{body}"}
        end

      {:ok, %Response{status_code: code, body: body}} ->
        {:error, "Guess failed: HTTP #{code} - #{body}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end

  # Entry point
  def main do
    player_name = "Supriya"

    with {:ok, sid, cookies} <- register_player(player_name),
         {:ok, cookies} <- create_game(sid, cookies),
         {:ok, _, _} <- make_guess(sid, "crane", cookies),
         {:ok, _, _} <- make_guess(sid, "stare", cookies) do
      IO.puts("🎉 Finished test run")
    else
      {:error, reason} ->
        IO.puts("❌ Error: #{inspect(reason)}")
    end
  end
end

WordleSolver.main()

