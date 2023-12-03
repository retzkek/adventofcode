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

  defparsec(:parse_game, game_header |> times(counts |> wrap, min: 1))

  def read_game(s) do
    {:ok, [game | draws], _, _, _, _} = parse_game(s)

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

  def draws_possible?(draws, cubes) do
    Enum.reduce(draws, true, fn draw, possible ->
      possible &&
        Enum.reduce(Map.keys(cubes), true, fn color, under ->
          under && Map.get(draw, color, 0) <= Map.get(cubes, color, 0)
        end)
    end)
  end

  def game_value({game, draws}, cubes) do
    if draws_possible?(draws, cubes) do
      game
    else
      0
    end
  end

  def sum_possible_games(lines, cubes) do
    Enum.map(lines, &read_game/1)
    |> Enum.reduce(0, fn x,acc -> acc+game_value(x,cubes) end)
  end

  def part1() do
    AOC.input(2023, 2, :lines)
    |> sum_possible_games(%{red: 12, green: 13, blue: 14})
    |> IO.puts
  end
end
