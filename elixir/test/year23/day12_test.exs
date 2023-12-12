defmodule AOC.Year23.Day12Test do
  use ExUnit.Case
  import AOC.Year23.Day12

  @test_input1 "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

  test "parser" do
    assert read_report(@test_input1) == [
             ["???.###", [1, 1, 3]],
             [".??..??...?##.", [1, 1, 3]],
             ["?#?#?#?#?#?#?#?", [1, 3, 1, 6]],
             ["????.#...#...", [4, 1, 1]],
             ["????.######..#####.", [1, 6, 5]],
             ["?###????????", [3, 2, 1]]
           ]

    assert count_broken_springs("#.#.###") == [1, 1, 3]
  end

  test "permutations" do
    assert spring_permutations("?.?.###") |> length() == 4
    assert possible_permutations("?.?.###", [1, 1, 3]) |> length() == 1
    assert possible_permutations(".??..??...?##.", [1, 1, 3]) |> length() == 4
    assert possible_permutations("?###????????", [3, 2, 1]) |> length() == 10
  end
end
