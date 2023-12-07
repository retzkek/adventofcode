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

  def score_hand(h, wild \\ nil) do
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
    |> then(fn score ->
      if wild do
        nwild = Enum.count(h, fn x -> x == wild end)

        case nwild do
          0 ->
            score

          1 ->
            case score do
              :four_kind -> :five_kind
              :three_kind -> :four_kind
              :two_pair -> :full_house
              :one_pair -> :three_kind
              :high_card -> :one_pair
            end

          2 ->
            case score do
              :full_house -> :five_kind
              :three_kind -> :five_kind
              :two_pair -> :four_kind
              :one_pair -> :three_kind
            end

          3 ->
            case score do
              :full_house -> :five_kind
              :three_kind -> :four_kind
            end

          _ ->
            :five_kind
        end
      else
        score
      end
    end)
  end

  def group_hands(hands, wild \\ nil) do
    Enum.group_by(hands, fn {h, _} -> score_hand(h, wild) end)
  end

  @hand_rank [:high_card, :one_pair, :two_pair, :three_kind, :full_house, :four_kind, :five_kind]

  def order_hands(hands, wild \\ nil) do
    groups = group_hands(hands, wild)

    Enum.flat_map(@hand_rank, fn rank ->
      (groups[rank] || [])
      |> Enum.sort(fn {a, _}, {b, _} ->
        Enum.zip(a, b)
        |> Enum.map(fn {x, y} ->
          {
            if x == wild do
              1
            else
              x
            end,
            if y == wild do
              1
            else
              y
            end
          }
        end)
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

  def winnings(hands, wild \\ nil) do
    order_hands(hands, wild)
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

  def part2() do
    AOC.input(2023, 7)
    |> read_hands()
    |> winnings(11)
    |> IO.puts()
  end
end
