defmodule AOC.Year23.Day14Test do
  use ExUnit.Case
  import AOC.Year23.Day14

  @test_input1 "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

  test "parser" do
    assert read_map(@test_input1) == {
             {10, 10},
             MapSet.new([
               {0, 0},
               {1, 0},
               {1, 2},
               {1, 3},
               {3, 0},
               {3, 1},
               {3, 4},
               {3, 9},
               {4, 1},
               {4, 7},
               {5, 0},
               {5, 5},
               {6, 2},
               {6, 6},
               {6, 9},
               {7, 7},
               {9, 1},
               {9, 2}
             ]),
             MapSet.new([
               {0, 5},
               {1, 4},
               {1, 9},
               {2, 5},
               {2, 6},
               {3, 3},
               {4, 8},
               {5, 2},
               {5, 7},
               {5, 9},
               {6, 5},
               {8, 0},
               {8, 5},
               {8, 6},
               {8, 7},
               {9, 0},
               {9, 5}
             ])
           }
  end

  test "roll north" do
    {_, moved, _} = read_map(@test_input1) |> roll_north()

    assert moved ==
             MapSet.new([
               {0, 0},
               {0, 1},
               {0, 2},
               {0, 3},
               {0, 7},
               {1, 0},
               {1, 1},
               {2, 0},
               {2, 1},
               {2, 4},
               {2, 9},
               {3, 0},
               {3, 5},
               {3, 6},
               {6, 2},
               {6, 7},
               {6, 9},
               {7, 2}
             ])

    assert read_map(@test_input1) |> roll_north() |> load() == 136
  end
end
