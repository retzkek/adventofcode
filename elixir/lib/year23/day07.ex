defmodule AOC.Year23.Day07 do
  import NimbleParsec

  whitespace = times(ascii_char([?\s]), min: 1)
  newline = ascii_char([?\n])

  hand = ascii_string([?A, ?K, ?Q, ?J, ?T, ?9, ?8, ?7, ?6, ?5, ?4, ?3, ?2, ?1], 5)

  defparsec(
    :parse_hands,
    times(
      hand
      |> ignore(whitespace)
      |> integer(min: 1)
      |> ignore(optional(newline))
      |> wrap(),
      min: 1
    )
  )

  def read_hands(s) do
    {:ok, rec, _, _, _, _} = parse_hands(s)

    Enum.map(rec, fn [hand, bid] ->
      {
        Enum.map(String.to_charlist(hand), fn x ->
          case x do
            ?A -> 14
            ?K -> 13
            ?Q -> 12
            ?J -> 11
            ?T -> 10
            _ -> x - ?0
          end
        end),
        bid
      }
    end)
  end

  def sort_hand(h) do
    Enum.frequencies(h)
    |> Map.values()
    |> Enum.sort()
  end

  def score_hand(h) do
    s = sort_hand(h)

    cond do
      s == [5] -> :five_kind
      Enum.max(s) == 4 -> :four_kind
      s == [2, 3] -> :full_house
      Enum.max(s) == 3 -> :three_kind
      s == [1, 2, 2] -> :two_pair
      s == [1, 1, 1, 2] -> :one_pair
      true -> :high_card
    end
  end

  def group_hands(hands) do
    Enum.group_by(hands, fn {h, _} -> score_hand(h) end)
  end

  @hand_rank [:high_card, :one_pair, :two_pair, :three_kind, :full_house, :four_kind, :five_kind]

  def order_hands(hands) do
    groups = group_hands(hands)

    Enum.flat_map(@hand_rank, fn rank ->
      (groups[rank] || [])
      |> Enum.sort(fn {a, _}, {b, _} ->
        Enum.zip(a, b)
        |> Enum.reduce_while(false, fn {x, y}, _ ->
          cond do
            x < y -> {:halt, true}
            x > y -> {:halt, false}
            x == y -> {:cont, true}
          end
        end)
      end)
      |> Enum.map(fn {_, bid} -> bid end)
    end)
  end

  def winnings(hands) do
    order_hands(hands)
    |> Stream.with_index()
    |> Stream.map(fn {bid, i} -> bid * (i + 1) end)
    # |> Enum.to_list()
    # |> IO.inspect()
    |> Enum.reduce(&+/2)
  end

  def part1() do
    AOC.input(2023, 7)
    |> read_hands()
    |> winnings()
    |> IO.puts()
  end
end
