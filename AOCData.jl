module AOCData

using Printf
using Downloads

function loadtoken()
    open(joinpath(homedir(),".config","aocd","token")) do f
        readline(f)
    end
end

function fetchinput(year::Int,day::Int,output::Union{AbstractString,IO})
    url = @sprintf("https://adventofcode.com/%d/day/%d/input", year, day)
    Downloads.download(url, output, headers=Dict("Cookie"=>"session="*loadtoken()))
end

function input(year::Int,day::Int)
    od = joinpath(homedir(),".config","aocd", string(year),string(day))
    if !ispath(od)
        mkpath(od)
    end
    of = joinpath(od,"input.txt")
    if !isfile(od)
        fetchinput(year,day,of)
    end
    open(of) do f
        readlines(f)
    end
end

function input(year::Int,day::Int,as::Type)
    map(x->parse(as,x), input(year,day))
end

end
