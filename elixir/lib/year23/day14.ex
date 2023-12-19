defmodule AOC.Year23.Day14 do
  import NimbleParsec

  defparsec(
    :parse_map,
    choice([
      ascii_char([?\#, ?O]) |> unwrap_and_tag(:sym),
      ignore(ascii_char([?.])),
      ignore(ascii_char([?\n]))
    ])
    |> pre_traverse(:tag_location)
    |> repeat
  )

  defp tag_location(rest, [], context, _, _) do
    {rest, [], context}
  end

  defp tag_location(rest, args, context, {line, line_offset}, offset) do
    # location as {row, col}
    {rest, [[loc: {line - 1, offset - line_offset}] ++ args], context}
  end

  def read_map(s) do
    {:ok, r, _, _, _, _} = parse_map(s)

    {
      # map dimensions
      Enum.reduce(r, {0, 0}, fn [{:loc, {row, col}}, _], {nr, nc} ->
        {
          max(row + 1, nr),
          max(col + 1, nc)
        }
      end),
      # movable rocks
      Enum.filter(r, fn x -> x[:sym] == ?O end)
      |> Enum.map(fn x -> x[:loc] end)
      |> MapSet.new(),
      # fixed rocks
      Enum.filter(r, fn x -> x[:sym] == ?\# end)
      |> Enum.map(fn x -> x[:loc] end)
      |> MapSet.new()
    }
  end

  def roll_north({dims, movable, fixed}) do
    {moved, any_moved?} =
      Enum.reduce(movable, {MapSet.new(), false}, fn {row, col}, {moved, any_moved?} ->
        if row > 0 &&
             !MapSet.member?(movable, {row - 1, col}) &&
             !MapSet.member?(fixed, {row - 1, col}) do
          {MapSet.put(moved, {row - 1, col}), true}
        else
          {MapSet.put(moved, {row, col}), any_moved?}
        end
      end)

    if any_moved? do
      roll_north({dims, moved, fixed})
    else
      {dims, moved, fixed}
    end
  end

  def load({{rows, _}, movable, _}) do
    Enum.map(movable, fn {row, _} -> rows - row end)
    |> Enum.reduce(&+/2)
  end

  def part1() do
    AOC.input(2023, 14)
    |> read_map()
    |> roll_north()
    |> load()
    |> IO.puts()
  end
end
