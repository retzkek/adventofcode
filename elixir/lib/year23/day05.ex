defmodule AOC.Year23.Day05 do
  import NimbleParsec

  whitespace = times(ascii_char([?\s]), min: 1)
  newline = ascii_char([?\n])

  map =
    unwrap_and_tag(integer(min: 1), :dest)
    |> ignore(whitespace)
    |> unwrap_and_tag(integer(min: 1), :src)
    |> ignore(whitespace)
    |> unwrap_and_tag(integer(min: 1), :length)
    |> ignore(optional(newline))
    |> wrap()

  defparsec(
    :parse_almanac,
    choice([
      ignore(string("seeds:"))
      |> times(
        ignore(whitespace)
        |> integer(min: 1),
        min: 1
      )
      |> tag(:seeds)
      |> ignore(newline),
      ignore(string("seed-to-soil map:\n"))
      |> times(map, min: 1)
      |> tag(:seed_to_soil),
      ignore(string("soil-to-fertilizer map:\n"))
      |> times(map, min: 1)
      |> tag(:soil_to_fertilizer),
      ignore(string("fertilizer-to-water map:\n"))
      |> times(map, min: 1)
      |> tag(:fertilizer_to_water),
      ignore(string("water-to-light map:\n"))
      |> times(map, min: 1)
      |> tag(:water_to_light),
      ignore(string("light-to-temperature map:\n"))
      |> times(map, min: 1)
      |> tag(:light_to_temperature),
      ignore(string("temperature-to-humidity map:\n"))
      |> times(map, min: 1)
      |> tag(:temperature_to_humidity),
      ignore(string("humidity-to-location map:\n"))
      |> times(map, min: 1)
      |> tag(:humidity_to_location),
      ignore(newline)
    ])
    |> repeat()
  )

  def read_almanac(s) do
    {:ok, alm, _, _, _, _} = parse_almanac(s)
    alm
  end

  def amap(src, mapping) do
    {dest, skip} =
      Enum.reduce_while(mapping, src, fn m, _ ->
        if src >= m[:src] && src <= m[:src] + m[:length] - 1 do
          {:halt, {m[:dest] + src - m[:src], m[:src] + m[:length] - src}}
        else
          {:cont, {src, 1}}
        end
      end)

    if skip == 1 do
      # find where the next highest range, if any, starts
      {dest,
       Enum.map(mapping, fn m -> m[:src] end)
       |> Enum.filter(fn x -> x > src end)
       |> then(fn x ->
         if length(x) == 0 do
           # there's no next range, so go big
           999_999_999
         else
           Enum.reduce(x, &min/2)
         end
       end)}
    else
      {dest, skip}
    end
  end

  def location_for_seed(almanac, seed) do
    steps = [
      almanac[:soil_to_fertilizer],
      almanac[:fertilizer_to_water],
      almanac[:water_to_light],
      almanac[:light_to_temperature],
      almanac[:temperature_to_humidity],
      almanac[:humidity_to_location]
    ]

    Enum.reduce(steps, amap(seed, almanac[:seed_to_soil]), fn step, {x, skip} ->
      {x2, skip2} = amap(x, step)
      {x2, min(skip, skip2)}
    end)
  end

  def min_seed_location(almanac) do
    Enum.map(almanac[:seeds], fn x ->
      {loc, _} = location_for_seed(almanac, x)
      loc
    end)
    |> Enum.reduce(&min/2)
  end

  def part1() do
    AOC.input(2023, 5)
    |> read_almanac()
    |> min_seed_location()
    |> IO.puts()
  end

  def locations_for_seed_range(almanac, [s, l]) do
    locations_for_seed_range(almanac, [s, l], s)
  end

  def locations_for_seed_range(almanac, [s, l], c) do
    # IO.puts("starting #{s}@#{c}")
    if c > s + l do
      []
    else
      {loc, skip} = location_for_seed(almanac, c)
      # IO.puts("... =#{loc}, seed+#{skip}")
      [loc | locations_for_seed_range(almanac, [s, l], c + skip)]
    end
  end

  def min_seed_range_location(almanac) do
    Enum.chunk_every(almanac[:seeds], 2)
    |> Enum.flat_map(fn x -> locations_for_seed_range(almanac, x) end)
    # |> IO.inspect()
    |> Enum.reduce(&min/2)
  end

  def part2() do
    AOC.input(2023, 5)
    |> read_almanac()
    |> min_seed_range_location()
    |> IO.puts()
  end
end
