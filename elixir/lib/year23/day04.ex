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
    #:io.format("~4B ~2B ~2B~n", [card[:id], length(card[:winning]), length(card[:numbers])])
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
    #|> IO.inspect()
    |> Enum.reduce(&+/2)
    |> IO.puts()
  end
end
