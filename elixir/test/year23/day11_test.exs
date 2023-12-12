defmodule AOC.Year23.Day11Test do
  use ExUnit.Case
  import AOC.Year23.Day11

  @test_input1 "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

  test "parser" do
    assert read_image(@test_input1) ==
             {{10, 10}, [{0, 3}, {1, 7}, {2, 0}, {4, 6}, {5, 1}, {6, 9}, {8, 7}, {9, 0}, {9, 4}]}
  end

  test "expansion" do
    img = read_image(@test_input1)
    assert empty_rows(img) == [3, 7]
    assert empty_cols(img) == [2, 5, 8]
    assert dist_between({5, 1}, {9, 4}, empty_rows(img), empty_cols(img)) == 9
    assert dist_between({0, 3}, {8, 7}, empty_rows(img), empty_cols(img)) == 15
    assert sum_dist_between(img) == 374
  end
end
