defmodule AOC.Year23.Day15Test do
  use ExUnit.Case
  import AOC.Year23.Day15

  @test_input1 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

  test "parser" do
    assert read_seq(@test_input1) == [
             "rn=1",
             "cm-",
             "qp=3",
             "cm=2",
             "qp-",
             "pc=4",
             "ot=9",
             "ab=5",
             "pc-",
             "pc=6",
             "ot=7"
           ]
  end

  test "hash" do
    assert Enum.map(read_seq(@test_input1), &hash/1) == [
             30,
             253,
             97,
             47,
             14,
             180,
             9,
             197,
             48,
             214,
             231
           ]
  end
end
