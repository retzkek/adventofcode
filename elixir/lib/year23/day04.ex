defmodule AOC.Year23.Day04 do
  import NimbleParsec

  defparsec(
    :parse_scratchcard,
    ignore(string("Card") |> repeat(ascii_char([?\s])))
    |> integer(min: 1)
    |> unwrap_and_tag(:id)
    |> ignore(string(":") |> repeat(ascii_char([?\s])))
    |> tag(
      repeat(
        integer(min: 1)
        |> repeat(ignore(ascii_char([?\s])))
      ),
      :winning
    )
    |> ignore(string("|") |> repeat(ascii_char([?\s])))
    |> tag(
      repeat(
        integer(min: 1)
        |> repeat(ignore(ascii_char([?\s])))
      ),
      :numbers
    )
  )

  def read_scratchcard(line) do
    {:ok, card, _, _, _, _} = parse_scratchcard(line)
    # :io.format("~4B ~2B ~2B~n", [card[:id], length(card[:winning]), length(card[:numbers])])
    card
  end

  def winning_numbers(card) do
    Enum.filter(card[:numbers], fn x -> x in card[:winning] end)
  end

  def points([]) do
    0
  end

  def points(ns) do
    Integer.pow(2, length(ns) - 1)
  end

  def part1() do
    AOC.input(2023, 4, :lines)
    |> Enum.map(fn x ->
      read_scratchcard(x)
      |> winning_numbers()
      |> points()
    end)
    # |> IO.inspect()
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end

  def win_scratchers(cards) do
    # this is ugly as sin, but it works and it's late...
    #
    # 1. Reduce over the cards, building up a map of the number of each card,
    # starting with 1 each.
    #
    # 2. For each winning ticket, iterate over a range from 1 to # wins, build
    # another map for each of the next ids giving them the number of copies of
    # the current winning card.
    #
    # 3. Merge the inner map with the outer by adding their counts.
    #
    # 4. Return the counts.
    Enum.reduce(
      cards,
      Enum.reduce(cards, %{}, fn x, init -> Map.put(init, x[:id], 1) end),
      fn x, copies ->
        win = winning_numbers(x) |> length

        if win > 0 do
          Enum.reduce(1..win, %{}, fn i, es ->
            id = i + x[:id]

            if id <= length(cards) do
              Map.put(es, id, copies[x[:id]])
            else
              es
            end
          end)
        else
          %{}
        end
        |> Map.merge(copies, fn _k, v1, v2 ->
          v1 + v2
        end)
      end
    )
    |> Map.values()
  end

  def part2() do
    AOC.input(2023, 4, :lines)
    |> Enum.map(&read_scratchcard/1)
    |> win_scratchers
    # |> IO.inspect()
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end
end
