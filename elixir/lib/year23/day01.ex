defmodule AOC.Year23.Day01 do
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

  def part1() do
    IO.puts(sum_calibrations(AOC.input(2023, 1, :lines), 1))
  end

  def part2() do
    IO.puts(sum_calibrations(AOC.input(2023, 1, :lines), 2))
  end
end

#AOC.Day01.main()
