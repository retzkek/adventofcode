defmodule AOC.Year23.Day08 do
  import NimbleParsec

  newline = ascii_char([?\n])

  node_id = ascii_string([?A..?Z] ++ [?0..?9], 3)

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

  def read_map(s) do
    {:ok, map, _, _, _, _} = parse_map(s)

    {
      # so we can iterate over it easily
      String.to_charlist(map[:directions]),
      Enum.reduce(map[:nodes], %{}, fn x, m ->
        Map.merge(m, %{
          (x[:id] <> "L") => x[:left],
          (x[:id] <> "R") => x[:right]
        })
      end)
    }
  end

  def walk_map({directions, nodes}, from \\ "AAA", to \\ "ZZZ") do
    Stream.cycle(directions)
    |> Enum.reduce_while({from, 1}, fn dir, {node, count} ->
      # IO.puts("from #{from} to #{to} at #{node}")
      next = nodes[node <> to_string([dir])]

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

  def ghost_starts(nodes) do
    Map.keys(nodes)
    |> Enum.filter(&String.ends_with?(&1, "AL"))
    |> Enum.map(&String.trim(&1, "L"))
  end

  def walk_end({directions, nodes}, start) do
    Stream.cycle(directions)
    |> Enum.reduce_while(
      {start, 1},
      fn dir, {last, count} ->
        lastdir = last <> to_string([dir])
        next = nodes[lastdir]

        if String.ends_with?(next, "Z") do
          {:halt,
           {
             next,
             count
           }}
        else
          {:cont,
           {
             next,
             count + 1
           }}
        end
      end
    )
    |> then(fn {_, count} -> count end)
  end

  def gcd(x, 0), do: x

  def gcd(x, y) do
    gcd(y, Integer.mod(x, y))
  end

  def lcm(ns) do
    Enum.reduce(ns, fn a, b -> div(a * b, gcd(a, b)) end)
  end

  def ghost_walk_map({directions, nodes}) do
    ghost_starts(nodes)
    |> IO.inspect()
    |> Enum.map(&walk_end({directions, nodes}, &1))
    |> IO.inspect()
    |> lcm()
  end

  def part2() do
    AOC.input(2023, 8)
    |> read_map()
    |> ghost_walk_map()
    |> IO.puts()
  end
end
