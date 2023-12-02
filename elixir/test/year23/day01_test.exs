defmodule AOC.Year23.Day01Test do
  use ExUnit.Case
  import AOC.Year23.Day01

  test "part 1" do
    assert calibration("onea2bthree", 1) == 22
    assert calibration("1abc2", 1) == 12
    assert calibration("pqr3stu8vwx", 1) == 38
    assert calibration("a1b2c3d4e5f", 1) == 15
    assert calibration("treb7uchet", 1) == 77
  end

  test "part 2" do
    assert calibration("two1nine", 2) == 29
    assert calibration("eightwothree", 2) == 83
    assert calibration("abcone2threexyz", 2) == 13
    assert calibration("xtwone3four", 2) == 24
    assert calibration("4nineeightseven2", 2) == 42
    assert calibration("zoneight234", 2) == 14
    assert calibration("7pqrstsixteen", 2) == 76
    assert calibration("7sevenine", 2) == 79
  end
end
