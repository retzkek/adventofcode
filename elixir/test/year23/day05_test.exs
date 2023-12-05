defmodule AOC.Year23.Day05Test do
  use ExUnit.Case
  import AOC.Year23.Day05

  @test_input "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

  test "parser" do
    alm = read_almanac(@test_input)
    assert alm[:seeds] == [79, 14, 55, 13]

    assert alm[:seed_to_soil] == [
             [dest: 50, src: 98, length: 2],
             [dest: 52, src: 50, length: 48]
           ]

    assert alm[:humidity_to_location] == [
             [dest: 60, src: 56, length: 37],
             [dest: 56, src: 93, length: 4]
           ]
  end

  test "mapping" do
    alm = read_almanac(@test_input)
    assert amap(79, alm[:seed_to_soil]) == {81, 19}
    assert amap(14, alm[:seed_to_soil]) == {14, 50}
    assert amap(55, alm[:seed_to_soil]) == {57, 43}
    assert amap(13, alm[:seed_to_soil]) == {13, 50}
  end

  test "location for seed" do
    alm = read_almanac(@test_input)
    assert location_for_seed(alm, 79) == {82, 3}
    assert location_for_seed(alm, 14) == {43, 8}
    assert location_for_seed(alm, 55) == {86, 4}
    assert location_for_seed(alm, 13) == {35, 2}
    assert min_seed_location(alm) == 35
    assert min_seed_range_location(alm) == 46
  end
end
