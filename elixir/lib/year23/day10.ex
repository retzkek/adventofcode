defmodule AOC.Year23.Day10 do
  import NimbleParsec
  use Memoize

  defparsec(
    :parse_sketch,
    choice([
      ascii_char([?|, ?-, ?L, ?J, ?7, ?F, ?S]) |> unwrap_and_tag(:symbol),
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

  def read_sketch(s) do
    {:ok, r, _, _, _, _} = parse_sketch(s)
    Enum.reduce(r, %{}, fn x, m -> Map.put(m, x[:loc], x[:symbol]) end)
  end

  defmemo find_start(sketch) do
    Enum.zip(Map.keys(sketch), Map.values(sketch))
    |> Enum.find(fn {_, sym} -> sym == ?S end)
    |> then(fn {loc, _} -> loc end)
  end

  def connections(sketch, {row, col}) do
    case sketch[{row, col}] do
      ?| ->
        [{row - 1, col}, {row + 1, col}]

      ?- ->
        [{row, col - 1}, {row, col + 1}]

      ?L ->
        [{row - 1, col}, {row, col + 1}]

      ?J ->
        [{row - 1, col}, {row, col - 1}]

      ?7 ->
        [{row + 1, col}, {row, col - 1}]

      ?F ->
        [{row + 1, col}, {row, col + 1}]

      ?S ->
        Enum.map(
          [{row - 1, col}, {row, col - 1}, {row, col + 1}, {row + 1, col}],
          fn loc ->
            {loc, connections(sketch, loc)}
          end
        )
        |> Enum.filter(fn {_, locs} -> Enum.find(locs, fn loc -> loc == {row, col} end) end)
        |> Enum.map(fn {loc, _} -> loc end)

      _ ->
        []
    end
  end

  def trace_pipes(sketch) do
    start = find_start(sketch)
    trace_pipes(sketch, [start], connections(sketch, start))
  end

  def trace_pipes(_, path, []) do
    path
  end

  def trace_pipes(sketch, path, nodes) do
    cons = connections(sketch, hd(nodes))
    trace_pipes(sketch, [hd(nodes) | path], Enum.reject(cons, fn x -> x in path end))
  end

  def part1() do
    AOC.input(2023, 10)
    |> read_sketch()
    |> trace_pipes()
    |> length()
    |> then(&div(&1, 2))
    |> IO.puts()
  end
end
