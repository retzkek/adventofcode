defmodule AOC.Year23.Day08Test do
  use ExUnit.Case
  import AOC.Year23.Day08
  alias AOC.Year23.Day08.Node, as: Node

  @test_input1 "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

  @test_input2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

  test "parser" do
    assert read_map(@test_input2) == {
             'LLR',
             %{
               "AAA" => %Node{left: "BBB", right: "BBB"},
               "BBB" => %Node{left: "AAA", right: "ZZZ"},
               "ZZZ" => %Node{left: "ZZZ", right: "ZZZ"}
             }
           }
  end

  test "walk map" do
    assert read_map(@test_input1) |> walk_map() == 2
    assert read_map(@test_input2) |> walk_map() == 6
  end
end
