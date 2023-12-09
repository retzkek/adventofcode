defmodule AOC.Year23.Day09Test do
  use ExUnit.Case
  import AOC.Year23.Day09

  @test_input1 "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

  test "parser" do
    assert read_report(@test_input1) == [
             [0, 3, 6, 9, 12, 15],
             [1, 3, 6, 10, 15, 21],
             [10, 13, 16, 21, 30, 45]
           ]
  end

  test "forecast" do
    assert next([0, 3, 6, 9, 12, 15]) == 18
    assert next([3, 3, 3, 3, 3]) == 3
    assert Enum.map(read_report(@test_input1), &next/1) == [18, 28, 68]
  end

  test "backcast" do
    assert prev([0, 3, 6, 9, 12, 15]) == -3
    assert prev([3, 3, 3, 3, 3]) == 3
    assert Enum.map(read_report(@test_input1), &prev/1) == [-3, 0, 5]
  end
end
