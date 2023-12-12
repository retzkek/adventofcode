defmodule AOC.Year23.Day11 do
  import NimbleParsec

  defparsec(
    :parse_image,
    choice([
      ascii_char([?\#]),
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

  def read_image(s) do
    {:ok, r, _, _, _, _} = parse_image(s)

    {
      Enum.reduce(r, {0, 0}, fn [{:loc, {row, col}}, _], {nr, nc} ->
        {
          max(row + 1, nr),
          max(col + 1, nc)
        }
      end),
      Enum.map(r, fn x -> x[:loc] end)
    }
  end

  def empty_rows({{nr, _}, gx}) do
    Enum.reject(0..(nr - 1), &Enum.find(gx, fn {r, _} -> r == &1 end))
  end

  def empty_cols({{_, nc}, gx}) do
    Enum.reject(0..(nc - 1), &Enum.find(gx, fn {_, c} -> c == &1 end))
  end

  def dist_between({r1, c1}, {r2, c2}, er, ec) do
    # taxi distance, with extra distance added for all empty rows and columns in between
    abs(r2 - r1) + Enum.count(r1..r2, &(&1 in er)) +
      abs(c2 - c1) + Enum.count(c1..c2, &(&1 in ec))
  end

  def dists_between_all([_], _, _) do
    []
  end

  def dists_between_all([g | gx], er, ec) do
    Enum.map(gx, &dist_between(g, &1, er, ec)) ++ dists_between_all(gx, er, ec)
  end

  def sum_dist_between({{nr, nc}, gx}) do
    dists_between_all(gx, empty_rows({{nr, nc}, gx}), empty_cols({{nr, nc}, gx}))
    # |> IO.inspect()
    |> Enum.reduce(&+/2)
  end

  def part1() do
    AOC.input(2023, 11)
    |> read_image()
    |> sum_dist_between()
    |> IO.puts()
  end
end
