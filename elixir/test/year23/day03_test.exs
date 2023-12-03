defmodule AOC.Year23.Day03Test do
  use ExUnit.Case
  import AOC.Year23.Day03

  @test_input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

  @test_input_short "467..114..
617*......"

  test "parser" do
    assert read_schematic(@test_input_short) == [
             [row: 0, col: 0, number: 467],
             [row: 0, col: 5, number: 114],
             [row: 1, col: 0, number: 617],
             [row: 1, col: 3, symbol: ?*]
           ]
  end

  test "number_length" do
    assert number_length(1) == 1
    assert number_length(12) == 2
    assert number_length(333) == 3
  end

  test "part_numbers" do
    assert read_schematic(@test_input_short) |> part_numbers() == [467, 617]

    assert read_schematic(@test_input) |> part_numbers() == [
             467,
             35,
             633,
             617,
             592,
             755,
             664,
             598
           ]

    assert read_schematic(@test_input) |> part_numbers() |> Enum.reduce(&+/2) == 4361
  end
end
