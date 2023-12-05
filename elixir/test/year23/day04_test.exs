defmodule AOC.Year23.Day04Test do
  use ExUnit.Case
  import AOC.Year23.Day04

  @test_input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" |> String.split("\n")

  test "parser" do
    assert read_scratchcard(hd(@test_input)) == [
             id: 1,
             winning: [41, 48, 83, 86, 17],
             numbers: [83, 86, 6, 31, 17, 9, 48, 53]
           ]
  end

  test "winning numbers" do
    assert read_scratchcard(hd(@test_input)) |> winning_numbers() == [83, 86, 17, 48]
  end

  test "points" do
    assert points([]) == 0
    assert points([1]) == 1
    assert points([1, 2]) == 2
    assert points([1, 2, 3]) == 4

    assert Enum.map(@test_input, fn x -> read_scratchcard(x) |> winning_numbers() |> points() end) ==
             [8, 2, 2, 1, 0, 0]
  end

  test "extra cards" do
    assert Enum.map(@test_input, &read_scratchcard/1) |> win_scratchers == [1, 2, 4, 8, 14, 1]
  end
end
