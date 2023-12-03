defmodule AOC.Year23.Day03 do
  import NimbleParsec

  defparsec(
    :parse_schematic,
    choice([
      # integer() does not match sign so that works ok for us, since it seems
      # part numbers cannot be negative
      integer(min: 1) |> unwrap_and_tag(:number),
      ignore(string(".")),
      ignore(string("\n")),
      choice([
        ascii_char([0..?0]),
        ascii_char([(?0 + 1)..(?A - 1)]),
        ascii_char([(?z + 1)..256])
      ])
      |> unwrap_and_tag(:symbol)
    ])
    |> pre_traverse(:tag_location)
    |> repeat
  )

  defp tag_location(rest, [], context, _, _) do
    {rest, [], context}
  end

  defp tag_location(rest, args, context, {line, line_offset}, offset) do
    {rest, [[row: line - 1, col: offset - line_offset] ++ args], context}
  end

  def read_schematic(s) do
    {:ok, sch, _, _, _, _} = parse_schematic(s)
    sch
  end

  def number_length(n), do: length(Integer.digits(n))

  def part_numbers(schematic) do
    numbers = Enum.filter(schematic, fn x -> x[:number] end)
    symbols = Enum.filter(schematic, fn x -> x[:symbol] end)

    Enum.filter(numbers, fn n ->
      nlen = number_length(n[:number])

      Enum.reduce(symbols, false, fn s, part? ->
        part? ||
          (s[:row] >= n[:row] - 1 && s[:row] <= n[:row] + 1 &&
             s[:col] >= n[:col] - 1 && s[:col] <= n[:col] + nlen)
      end)
    end)
    |> Enum.map(fn x -> x[:number] end)
  end

  def part1() do
    AOC.input(2023, 3)
    |> read_schematic()
    # |> IO.inspect()
    |> part_numbers()
    # |> IO.inspect()
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end

  def gear_ratios(schematic) do
    numbers = Enum.filter(schematic, fn x -> x[:number] end)
    gears = Enum.filter(schematic, fn x -> x[:symbol] == ?* end)

    Enum.flat_map(gears, fn g ->
      Enum.filter(numbers, fn n ->
        g[:row] >= n[:row] - 1 && g[:row] <= n[:row] + 1 &&
          g[:col] >= n[:col] - 1 && g[:col] <= n[:col] + number_length(n[:number])
      end)
      |> then(fn ns ->
        if length(ns) == 2 do
          [Enum.reduce(ns, 1, fn n, ratio -> ratio * n[:number] end)]
        else
          []
        end
      end)
    end)
  end

  def part2() do
    AOC.input(2023, 3)
    |> read_schematic()
    |> gear_ratios()
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end
end
