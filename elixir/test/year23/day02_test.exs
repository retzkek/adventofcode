defmodule AOC.Year23.Day02Test do
  use ExUnit.Case
  import AOC.Year23.Day02

  test "game parser" do
    assert parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") ==
             {:ok,
              [
                1,
                [[3, "blue"], [4, "red"]],
                [[1, "red"], [2, "green"], [6, "blue"]],
                [[2, "green"]]
              ], "", %{}, {1, 0}, 54}
  end

  test "game reader" do
    assert read_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") ==
             {1,
              [
                %{blue: 3, red: 4, green: 0},
                %{red: 1, green: 2, blue: 6},
                %{green: 2, red: 0, blue: 0}
              ]}
  end

  test "game possible" do
    cubes = %{red: 12, green: 13, blue: 14}

    assert read_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
           |> elem(1)
           |> draws_possible?(cubes) == true

    assert read_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
           |> elem(1)
           |> draws_possible?(cubes) == true

    assert read_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
           |> elem(1)
           |> draws_possible?(cubes) == false

    assert read_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
           |> elem(1)
           |> draws_possible?(cubes) == false

    assert read_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
           |> elem(1)
           |> draws_possible?(cubes) == true
  end

  test "game value" do
    cubes = %{red: 12, green: 13, blue: 14}

    assert read_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
           |> game_value(cubes) == 1

    assert read_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
           |> game_value(cubes) == 2

    assert read_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
           |> game_value(cubes) == 0

    assert read_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
           |> game_value(cubes) == 0

    assert read_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
           |> game_value(cubes) == 5
  end

  test "part 1" do
    assert sum_possible_games([
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ], %{red: 12, green: 13, blue: 14}) == 8
  end

  test "min cubes required" do
    assert read_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
           |> min_cubes == %{red: 4, green: 2, blue: 6}

    assert read_game("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
           |> min_cubes == %{red: 1, green: 3, blue: 4}

    assert read_game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
           |> min_cubes == %{red: 20, green: 13, blue: 6}

    assert read_game("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
           |> min_cubes == %{red: 14, green: 3, blue: 15}

    assert read_game("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
           |> min_cubes == %{red: 6, green: 3, blue: 2}

  end

  test "cube power" do
    assert power(%{red: 4, green: 2, blue: 6}) == 48
  end


end
