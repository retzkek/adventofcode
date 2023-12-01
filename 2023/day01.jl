using AOCData
using Test
using Chain

function calibration(s)
    m = collect(eachmatch(r"\d",s))
    s = first(m).match*last(m).match
    parse(Int,s)
end


@testset "calibration" begin
    @test calibration("1abc2") == 12
    @test calibration("pqr3stu8vwx") == 38
    @test calibration("a1b2c3d4e5f") == 15
    @test calibration("treb7uchet") == 77
end

function part1()
    AOCData.input(2023,1) .|> calibration |> sum |> println
end

@time part1()

function calibration2(s)
    @chain s begin
        eachmatch(r"\d|one|two|three|four|five|six|seven|eight|nine",_,overlap=true)
        collect(_)
        first(_).match*last(_).match
        replace(_,
                "one"=>1,
                "two"=>2,
                "three"=>3,
                "four"=>4,
                "five"=>5,
                "six"=>6,
                "seven"=>7,
                "eight"=>8,
                "nine"=>9)
        parse(Int,_)
    end
end


@testset "calibration2" begin
    @test calibration2("two1nine") == 29
    @test calibration2("eightwothree") == 83
    @test calibration2("abcone2threexyz") == 13
    @test calibration2("xtwone3four") == 24
    @test calibration2("4nineeightseven2") == 42
    @test calibration2("zoneight234") == 14
    @test calibration2("7pqrstsixteen") == 76
    # missing test case imo
    @test calibration2("7sevenine") == 79
end

function part2()
    #for c in AOCData.input(2023,1)
    #    @debug c calibration2(c)
    #end
    AOCData.input(2023,1) .|> calibration2 |> sum |> println
end

@time part2()
