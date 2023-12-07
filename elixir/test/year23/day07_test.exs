defmodule AOC.Year23.Day07Test do
  use ExUnit.Case
  import AOC.Year23.Day07

  @test_input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

  test "parser" do
    assert read_hands(@test_input) == [
             {[3, 2, 10, 3, 13], 765},
             {[10, 5, 5, 11, 5], 684},
             {[13, 13, 6, 7, 7], 28},
             {[13, 10, 11, 11, 10], 220},
             {[12, 12, 12, 11, 14], 483}
           ]
  end

  test "score hands" do
    assert sort_hand([2, 3, 4, 4, 5]) == [1, 1, 1, 2]

    assert score_hand([2, 2, 2, 2, 2]) == :five_kind
    assert score_hand([3, 2, 2, 2, 2]) == :four_kind
    assert score_hand([3, 3, 2, 2, 2]) == :full_house
    assert score_hand([3, 4, 2, 2, 2]) == :three_kind
    assert score_hand([3, 3, 2, 2, 4]) == :two_pair
    assert score_hand([3, 3, 2, 4, 5]) == :one_pair
    assert score_hand([2, 3, 4, 5, 6]) == :high_card

    assert score_hand([3, 2, 10, 3, 13], 11) == :one_pair
    assert score_hand([10, 5, 5, 11, 5], 11) == :four_kind
    assert score_hand([13, 13, 6, 7, 7], 11) == :two_pair
    assert score_hand([13, 10, 11, 11, 10], 11) == :four_kind
    assert score_hand([12, 12, 12, 11, 14], 11) == :four_kind
  end

  test "group hands" do
    assert group_hands(read_hands(@test_input)) == %{
             :one_pair => [
               {[3, 2, 10, 3, 13], 765}
             ],
             :two_pair => [
               {[13, 13, 6, 7, 7], 28},
               {[13, 10, 11, 11, 10], 220}
             ],
             :three_kind => [
               {[10, 5, 5, 11, 5], 684},
               {[12, 12, 12, 11, 14], 483}
             ]
           }
  end

  test "order hands" do
    assert order_hands(read_hands(@test_input)) == [765, 220, 28, 684, 483]
    assert order_hands(read_hands(@test_input), 11) == [765, 28, 684, 483, 220]
  end

  test "winnings" do
    assert winnings(read_hands(@test_input)) == 6440
    assert winnings(read_hands(@test_input), 11) == 5905
  end
end
