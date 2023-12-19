defmodule AOC.Year23.Day15 do
  import NimbleParsec

  seq =
    ascii_string([?a..?z] ++ [?-, ?=] ++ [?0..?9], min: 3)
    |> ignore(optional(ascii_char([?,])))

  defparsec(
    :parse_seq,
    times(seq, min: 1)
  )

  def read_seq(s) do
    {:ok, r, _, _, _, _} = parse_seq(s)
    r
  end

  def hash(str) do
    Enum.reduce(to_charlist(str), 0, fn x, cv -> rem((cv + x) * 17, 256) end)
  end

  def part1() do
    AOC.input(2023, 15)
    |> read_seq()
    |> Enum.map(&hash/1)
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end
end
