defmodule AOC.Year23.Day06Test do
  use ExUnit.Case
  import AOC.Year23.Day06

  @test_input "Time:      7  15   30
Distance:  9  40  200"

  test "parser" do
    rec = read_records(@test_input)
    assert rec == [time: [7, 15, 30], distance: [9, 40, 200]]
  end

  test "beat record" do
    assert beat_record(7, 9) == 4
    assert beat_record(15, 40) == 8
    assert beat_record(30, 200) == 9
    assert beat_record(30, 200) == 9
  end

  test "fix record" do
    rec = fix_record(read_records(@test_input))
    assert rec == {71530, 940_200}
    assert beat_record(rec) == 71503
  end
end
