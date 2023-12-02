defmodule AOC.Year23.Day02 do
  import NimbleParsec

  game_header =
    ignore(string("Game "))
    |> integer(min: 1)
    |> ignore(string(": "))

  count =
    integer(min: 1)
    |> ignore(string(" "))
    |> choice([
      string("red"),
      string("green"),
      string("blue")
    ])
    |> optional(ignore(string(", ")))

  counts =
    times(count |> wrap, min: 1, max: 3)
    |> optional(ignore(string("; ")))

  defparsec(:parse_line, game_header |> times(counts |> wrap, min: 1))

  def parse_game(s) do
    {:ok, [game | draws], _, _, _, _} = parse_line(s)

    {
      game,
      Enum.map(draws, fn draw ->
        Enum.reduce(draw, %{red: 0, green: 0, blue: 0}, fn [n, color], m ->
          case color do
            "red" -> Map.put(m, :red, n)
            "green" -> Map.put(m, :green, n)
            "blue" -> Map.put(m, :blue, n)
          end
        end)
      end)
    }
  end

  def main() do
    IO.puts("hello")
  end
end
