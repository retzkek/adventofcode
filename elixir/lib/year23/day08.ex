defmodule AOC.Year23.Day08 do
  import NimbleParsec

  newline = ascii_char([?\n])

  node_id = ascii_string([?A..?Z], 3)

  node =
    unwrap_and_tag(node_id, :id)
    |> ignore(string(" = ("))
    |> unwrap_and_tag(node_id, :left)
    |> ignore(string(", "))
    |> unwrap_and_tag(node_id, :right)
    |> ignore(concat(string(")"), optional(newline)))
    |> wrap()

  defparsec(
    :parse_map,
    unwrap_and_tag(ascii_string([?R, ?L], min: 1), :directions)
    |> ignore(times(newline, min: 1))
    |> tag(times(node, min: 1), :nodes)
  )

  defmodule Node do
    defstruct left: nil, right: nil
  end

  def read_map(s) do
    {:ok, map, _, _, _, _} = parse_map(s)

    {
      # so we can iterate over it easily
      String.to_charlist(map[:directions]),
      Map.new(map[:nodes], fn x -> {x[:id], %Node{left: x[:left], right: x[:right]}} end)
    }
  end

  def walk_map({directions, nodes}, from \\ "AAA", to \\ "ZZZ") do
    Stream.cycle(directions)
    |> Enum.reduce_while({from, 1}, fn dir, {node, count} ->
      # IO.puts("from #{from} to #{to} at #{node}")
      next =
        if dir == ?L do
          nodes[node].left
        else
          nodes[node].right
        end

      if next == to do
        {:halt, {to, count}}
      else
        {:cont, {next, count + 1}}
      end
    end)
    |> then(fn {_, count} -> count end)
  end

  def part1() do
    AOC.input(2023, 8)
    |> read_map()
    |> walk_map()
    |> IO.puts()
  end
end
