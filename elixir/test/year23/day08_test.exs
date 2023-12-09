defmodule AOC.Year23.Day08Test do
  use ExUnit.Case
  import AOC.Year23.Day08

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
               "AAAL" => "BBB",
               "AAAR" => "BBB",
               "BBBL" => "AAA",
               "BBBR" => "ZZZ",
               "ZZZL" => "ZZZ",
               "ZZZR" => "ZZZ"
             }
           }
  end

  test "walk map" do
    assert read_map(@test_input1) |> walk_map() == 2
    assert read_map(@test_input2) |> walk_map() == 6
  end

  @test_input3 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

  test "ghost walk" do
    assert read_map(@test_input3) |> then(fn {_, nodes} -> ghost_starts(nodes) end) == [
             "11A",
             "22A"
           ]

    assert read_map(@test_input3) |> ghost_walk_map() == 6
    assert read_map(@test_input3) |> walk_end("11A") == 2
    assert read_map(@test_input3) |> walk_end("22A") == 3
  end

  test "gcd" do
    assert gcd(1071, 462) == 21
  end

  test "least common multiple" do
    assert lcm([4, 6]) == 12
    assert lcm([21, 6]) == 42
    assert lcm([8, 9, 21]) == 504
  end
end
