defmodule AOC.Year23.Day02Test do
  use ExUnit.Case
  import AOC.Year23.Day02

  test "line parser" do
    assert parse_line("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") ==
             {:ok,
              [
                1,
                [[3, "blue"], [4, "red"]],
                [[1, "red"], [2, "green"], [6, "blue"]],
                [[2, "green"]]
              ], "", %{}, {1, 0}, 54}
  end

  test "game parser" do
    assert parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") ==
             {1, [%{blue: 3, red: 4, green: 0}, %{red: 1, green: 2, blue: 6}, %{green: 2, red: 0, blue: 0}]}

    # assert parse_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue") == [2, []]
    # assert parse_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red") == [3, []]
    # assert parse_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red") == [4, []]
    # assert parse_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green") == [5, []]
  end
end
