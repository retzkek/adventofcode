Code.require_file("aoc.exs")

defmodule AdventOfCode.Day01 do
  defp first_digit(s, 1) do
    [x] = Regex.run(~r/\d/, s)
    String.to_integer(x)
  end

  defp first_digit(s, 2) do
    case Regex.run(~r/\d|one|two|three|four|five|six|seven|eight|nine/, s) do
      ["one"] -> 1
      ["two"] -> 2
      ["three"] -> 3
      ["four"] -> 4
      ["five"] -> 5
      ["six"] -> 6
      ["seven"] -> 7
      ["eight"] -> 8
      ["nine"] -> 9
      [x] -> String.to_integer(x)
    end
  end

  defp last_digit(s, 1) do
    [x] = Regex.run(~r/\d/, String.reverse(s))
    String.to_integer(x)
  end

  defp last_digit(s, 2) do
    case Regex.run(~r/\d|enin|thgie|neves|xis|evif|ruof|eerht|owt|eno/, String.reverse(s)) do
      ["eno"] -> 1
      ["owt"] -> 2
      ["eerht"] -> 3
      ["ruof"] -> 4
      ["evif"] -> 5
      ["xis"] -> 6
      ["neves"] -> 7
      ["thgie"] -> 8
      ["enin"] -> 9
      [x] -> String.to_integer(x)
    end
  end

  def calibration(s, p), do: 10 * first_digit(s, p) + last_digit(s, p)

  def sum_calibrations(cs, p) do
    Enum.map(cs, fn x -> calibration(x, p) end)
    |> Enum.reduce(&+/2)
  end

  def main() do
    IO.puts(sum_calibrations(AdventOfCode.input(2023, 1, :lines), 1))
    IO.puts(sum_calibrations(AdventOfCode.input(2023, 1, :lines), 2))
  end
end

ExUnit.start()

defmodule AdventOfCode.Day01Test do
  use ExUnit.Case
  import AdventOfCode.Day01

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

AdventOfCode.Day01.main()
