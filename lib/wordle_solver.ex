defmodule WordleSolver do
  use GenServer
  require Logger

  @register_url "http://localhost:8000/register"
  @create_url "http://localhost:8000/create"
  @guess_url "http://localhost:8000/guess"

  # --- Entry Point ---

  def main do
    {:ok, _pid} = WordleSolver.start_link("supriya")
    WordleSolver.run_solver()

    # keep process alive so GenServer keeps running
    :timer.sleep(:infinity)
  end

  # --- Client API ---

  def start_link(username) do
    GenServer.start_link(__MODULE__, username, name: __MODULE__)
  end

  def run_solver do
    GenServer.cast(__MODULE__, :start_game)
  end

  # --- Server Callbacks ---

  def init(username) do
    case register_player(username) do
      {:ok, player_id, cookie} ->
        Logger.info("Registered player '#{username}' with ID: #{player_id}")
        {:ok, %{player_id: player_id, cookie: cookie, words: load_words()}}

      {:error, reason} ->
        Logger.error("Failed to register: #{inspect(reason)}")
        {:stop, reason}
    end
  end

  def handle_cast(:start_game, state) do
    case create_game(state) do
      {:ok, game_id} ->
        Logger.info("Started new game #{game_id}")
        play_loop(game_id, state)

      {:error, reason} ->
        Logger.error("Game creation failed: #{inspect(reason)}")
    end

    {:noreply, state}
  end

  # --- Internal Functions ---

  defp register_player(username) do
    body = Jason.encode!(%{name: username})

    case HTTPoison.post(@register_url, body, [{"Content-Type", "application/json"}]) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp, headers: headers}} ->
        %{"id" => player_id} = Jason.decode!(resp)

        cookie =
          headers
          |> Enum.find(fn {k, _} -> String.downcase(k) == "set-cookie" end)
          |> case do
            {_, val} -> val
            nil -> nil
          end

        {:ok, player_id, cookie}

      {:ok, %HTTPoison.Response{status_code: code, body: resp}} ->
        {:error, %{status: code, body: resp}}

      {:error, err} ->
        {:error, err}
    end
  end

  defp create_game(%{cookie: cookie}) do
    headers =
      if cookie do
        [{"Cookie", cookie}]
      else
        []
      end

    case HTTPoison.post(@create_url, "{}", headers) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp}} ->
        %{"id" => game_id} = Jason.decode!(resp)
        {:ok, game_id}

      {:ok, %HTTPoison.Response{status_code: code, body: resp}} ->
        {:error, %{status: code, body: resp}}

      {:error, err} ->
        {:error, err}
    end
  end

  defp make_guess(game_id, guess, %{cookie: cookie}) do
    headers = [{"Content-Type", "application/json"}]

    headers =
      if cookie do
        [{"Cookie", cookie} | headers]
      else
        headers
      end

    body = Jason.encode!(%{id: game_id, guess: guess})

    case HTTPoison.post(@guess_url, body, headers) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp}} ->
        Jason.decode!(resp)

      {:ok, %HTTPoison.Response{status_code: code, body: resp}} ->
        {:error, %{status: code, body: resp}}

      {:error, err} ->
        {:error, err}
    end
  end

  defp play_loop(game_id, state) do
    Enum.reduce_while(1..6, state.words, fn attempt, words ->
      case words do
        [] ->
          Logger.warning("⚠️ Ran out of candidate words — restarting game.")
          {:halt, []}

        _ ->
          [guess | rest] = words
          Logger.info("Attempt ##{attempt}: Is it '#{guess}'?")

          case make_guess(game_id, guess, state) do
            %{"feedback" => fb, "message" => msg} ->
              Logger.info("Feedback: #{fb} | Message: #{msg}")

              if fb == "GGGGG" do
                Logger.info("🎉 Solved in #{attempt} tries!")
                {:halt, rest}
              else
                {:cont, rest}
              end

            {:error, err} ->
              Logger.error("❌ Error making guess: #{inspect(err)}")
              {:halt, rest}
          end
      end
    end)
  end

  defp load_words do
    case File.read("5words.txt") do
      {:ok, contents} ->
        contents
        |> String.split("\n", trim: true)

      {:error, :enoent} ->
        Logger.error("Error: 5words.txt not found.")
        []

      {:error, reason} ->
        Logger.error("Failed to read words file: #{inspect(reason)}")
        []
    end
  end
end

