defmodule AOC.Year23.Day09 do
  import NimbleParsec

  newline = ascii_char([?\n])
  whitespace = times(ascii_char([?\s]), min: 1)

  # negative numbers!
  history =
    ascii_string([?-] ++ Enum.to_list(?0..?9), min: 1)
    |> ignore(optional(whitespace))
    |> times(min: 1)
    |> map({String, :to_integer, []})
    |> wrap()
    |> ignore(optional(newline))

  defparsec(
    :parse_report,
    times(history, min: 1)
  )

  def read_report(s) do
    {:ok, r, _, _, _, _} = parse_report(s)
    r
  end

  def next(ns) do
    if Enum.all?(ns, &(&1 == 0)) do
      0
    else
      Enum.reduce(tl(ns), {hd(ns), []}, fn x, {last, diffs} ->
        {x, [x - last | diffs]}
      end)
      |> then(fn {_, diffs} -> List.last(ns) + next(Enum.reverse(diffs)) end)
    end
  end

  def part1() do
    AOC.input(2023, 9)
    |> read_report()
    |> Enum.map(&next/1)
    |> IO.inspect()
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end
end
