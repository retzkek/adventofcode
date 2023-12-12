defmodule AOC.Year23.Day12 do
  import NimbleParsec

  newline = ascii_char([?\n])
  whitespace = times(ascii_char([?\s]), min: 1)

  row =
    ascii_string([?\#, ?., ??], min: 1)
    |> ignore(whitespace)
    |> wrap(times(integer(min: 1) |> ignore(optional(ascii_char([?,]))), min: 1))
    |> ignore(optional(newline))

  defparsec(
    :parse_report,
    times(wrap(row), min: 1)
  )

  def read_report(s) do
    {:ok, r, _, _, _, _} = parse_report(s)
    r
  end

  defparsec(
    :parse_springs,
    choice([
      ascii_string([?\#], min: 1),
      ignore(ascii_string([?.], min: 1))
    ])
    |> times(min: 1)
  )

  def count_broken_springs(s) do
    {:ok, r, _, _, _, _} = parse_springs(s)

    Enum.map(r, &String.length/1)
  end

  def spring_permutations([]), do: [[]]

  def spring_permutations(s) when is_bitstring(s), do: spring_permutations(String.to_charlist(s))

  def spring_permutations([c | s]) do
    if c == ?? do
      spring_permutations([?\# | s]) ++ spring_permutations([?. | s])
    else
      Enum.map(spring_permutations(s), fn x -> [c | x] end)
    end
  end

  def possible_permutations(s, broken) do
    Enum.filter(spring_permutations(s), &(count_broken_springs(to_string(&1)) == broken))
  end

  def part1() do
    AOC.input(2023, 12)
    |> read_report()
    |> Enum.map(fn [s, b] -> possible_permutations(s, b) |> length() end)
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end
end
