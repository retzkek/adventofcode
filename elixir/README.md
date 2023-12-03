# AOC

[Advent of Code](https://adventofcode.com/) solutions in Elixir, structured as a mix package for dependencies and testing. 

* Problems are in modules named like `AOC.YearYY.DayDD` in `lib/yearYY/dayDD.ex`. 
* Tests are in `test/yearYY/dayDD_test.exs`.
* General helper libraries are in module `AOC`.

Run `mix test` to run **all** tests. To test a specific day:

    mix test test/yearYY/dayDD_test.ex
  
To run a single problem:

    mix run -e AOC.YearYY.DayDD.partN
    
or whatever the entrypoint function was named for that problem.

## Helpers

The `AOC` module contains helper functions to download and cache problem input.
Put your cookie session key into `~/.config/aocd/token`. Then you can call
`AOC.input(year, day)` to get the input in a single string, where `year` is the
four-digit year and `day` is the unpadded day of the month. Call
`AOC.input(year, day, :lines)` to get the input split into a list of lines.
