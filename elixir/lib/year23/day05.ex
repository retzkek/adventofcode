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
    Enum.reduce(mapping, src, fn m, dest ->
      cond do
        dest != src ->
          dest

        src >= m[:src] && src <= m[:src] + m[:length] - 1 ->
          m[:dest] + src - m[:src]

        true ->
          src
      end
    end)
  end

  def location_for_seed(almanac, seed) do
    amap(seed, almanac[:seed_to_soil])
    |> amap(almanac[:soil_to_fertilizer])
    |> amap(almanac[:fertilizer_to_water])
    |> amap(almanac[:water_to_light])
    |> amap(almanac[:light_to_temperature])
    |> amap(almanac[:temperature_to_humidity])
    |> amap(almanac[:humidity_to_location])
  end

  def min_seed_location(almanac) do
    Enum.map(almanac[:seeds], fn x -> location_for_seed(almanac, x) end)
    |> Enum.reduce(&min/2)
  end

  def part1() do
    AOC.input(2023, 5)
    |> read_almanac()
    |> min_seed_location()
    |> IO.puts()
  end
end
