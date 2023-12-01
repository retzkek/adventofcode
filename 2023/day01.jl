using AOCData
using Test

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

part1()
