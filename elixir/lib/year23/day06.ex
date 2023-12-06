defmodule AOC.Year23.Day06 do
  import NimbleParsec

  whitespace = times(ascii_char([?\s]), min: 1)
  newline = ascii_char([?\n])

  defparsec(
    :parse_records,
    concat(
      ignore(string("Time:"))
      |> times(
        ignore(whitespace)
        |> integer(min: 1),
        min: 1
      )
      |> ignore(newline)
      |> tag(:time),
      ignore(string("Distance:"))
      |> times(
        ignore(whitespace)
        |> integer(min: 1),
        min: 1
      )
      |> tag(:distance)
    )
  )

  def read_records(s) do
    {:ok, rec, _, _, _, _} = parse_records(s)
    rec
  end

  def beat_record(time, distance) do
    Enum.map(1..time, fn x -> (time - x) * x end)
    |> Enum.filter(fn x -> x > distance end)
    # |> IO.inspect()
    |> length()
  end

  def beat_record({time, distance}), do: beat_record(time, distance)

  def part1() do
    AOC.input(2023, 6)
    |> read_records()
    |> then(fn rec -> Enum.zip(rec[:time], rec[:distance]) end)
    # |> IO.inspect()
    |> Enum.map(&beat_record/1)
    |> Enum.reduce(1, &*/2)
    |> IO.puts()
  end
end
