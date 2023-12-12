defmodule AOC.Year23.Day10Test do
  use ExUnit.Case
  import AOC.Year23.Day10

  @test_input1 "-L|F7
7S-7|
L|7||
-L-J|
L|-JF"

  @test_input2 "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"

  test "parser" do
    assert read_sketch(@test_input1) |> Map.get({1,1}) == ?S
    assert read_sketch(@test_input2) |> Map.get({2,0}) == ?S
  end

  test "sketch" do
    assert read_sketch(@test_input1) |> find_start() == {1, 1}
    assert read_sketch(@test_input2) |> find_start() == {2, 0}
  end

  test "connections" do
    sk = read_sketch(@test_input1)
    assert connections(sk, find_start(sk)) == [{1, 2}, {2, 1}]
    assert trace_pipes(sk) == [{2, 1}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {1, 3}, {1, 2}, {1, 1}]

    assert length(trace_pipes(read_sketch(@test_input2))) == 16
  end

end
