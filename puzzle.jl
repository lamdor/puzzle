using InlineTest
using StaticArrays
using Random

@enum SideType begin
    outArrow
    inArrow
    octagon
    cross
end

struct Side
    type::SideType
    cutout::Bool
end

matches(s1::Side, s2::Side) = s1.type == s2.type && s1.cutout != s2.cutout

@testset "matches" begin
    @test matches(Side(outArrow, false), Side(outArrow, true))
    @test matches(Side(cross, false), Side(cross, true))
    @test !matches(Side(inArrow, false), Side(outArrow, true))
    @test !matches(Side(inArrow, false), Side(octagon, true))
end

struct Piece
    # index 1 is the top side
    sides::SVector{4,Side}
    Piece(sides::Side...) = new(collect(sides))
    Piece(sides::SVector{4,Side}) = new(sides)
end

Base.copy(p::Piece) = Piece(Base.copy(p.sides))

function rotate(p::Piece)
    return Piece(SVector{4}(p.sides[2:4]..., p.sides[1]))
end

@testset "rotate" begin
    p = Piece(
        Side(inArrow, false),
        Side(octagon, true),
        Side(outArrow, true),
        Side(outArrow, false),
    )
    rotated = rotate(p)
    @test rotated.sides == SVector{4}(
        Side(octagon, true),
        Side(outArrow, true),
        Side(outArrow, false),
        Side(inArrow, false),
    )
end

function flip(p::Piece)
    return Piece(SVector{4}(
        p.sides[1], 
        p.sides[4], 
        p.sides[3], 
        p.sides[2],
    ))
end

@testset "flip" begin
    p = Piece(
        Side(inArrow, false),
        Side(octagon, true),
        Side(outArrow, true),
        Side(outArrow, false),
    )
    flipped = flip(p)
    @test flipped.sides == SVector{4}(
        Side(inArrow, false),
        Side(outArrow, false),
        Side(outArrow, true),
        Side(octagon, true),
    )
end

function combinations(p::Piece)
    combos = Piece[]
    for _ in 1:4
        p = rotate(p)
        push!(combos, Base.copy(p))
    end
    p = flip(p)
    for _ in 1:4
        p = rotate(p)
        push!(combos, Base.copy(p))
    end
    return combos
end

@testset "combinations" begin
    p = Piece(
        Side(cross, false),
        Side(inArrow, false),
        Side(inArrow, true),
        Side(outArrow, true),
    )
    combos = combinations(p)
    @test (p in combos)
    @test (flip(p) in combos)
    @test (rotate(rotate(flip(p))) in combos)
    @test (flip(rotate(rotate(flip(p)))) in combos)
end

const Puzzle = Matrix{Union{Nothing,Piece}}

function get(puzz::Puzzle, i::Int, j::Int)
    if i <= 0 || j <= 0 || i >= 5 || j >= 5
        return nothing
    else
        return puzz[i, j]
    end
end

@testset "get" begin
    p = Piece(
        Side(inArrow, false),
        Side(octagon, true),
        Side(outArrow, true),
        Side(outArrow, false),
    )
    puzz = Puzzle([p for i in 1:4, j in 1:4])
    @test isnothing(get(puzz, 0, 0))
    @test isnothing(get(puzz, 1, 0))
    @test isnothing(get(puzz, 0, 1))
    @test isnothing(get(puzz, 5, 1))
    @test isnothing(get(puzz, 1, 5))
    @test isnothing(get(puzz, 5, 5))
    @test get(puzz, 1, 1) == p
    @test get(puzz, 1, 3) == p
end

function fits(p::Piece, puzz::Puzzle, i::Int, j::Int)
    isnothing(puzz[i, j]) || return false
    top = get(puzz, i - 1, j)
    right = get(puzz, i, j + 1)
    bottom = get(puzz, i + 1, j)
    left = get(puzz, i, j - 1)

    (isnothing(top) || matches(top.sides[3], p.sides[1])) &&
        (isnothing(left) || matches(left.sides[2], p.sides[4])) &&
        (isnothing(right) || matches(right.sides[4], p.sides[2])) &&
        (isnothing(bottom) || matches(bottom.sides[1], p.sides[3]))
end

emptyPuzzle()::Puzzle = [
    nothing nothing nothing nothing
    nothing nothing nothing nothing
    nothing nothing nothing nothing
    nothing nothing nothing nothing
]

@testset "fits" begin
    other = Piece(
        Side(inArrow, true),
        Side(inArrow, true),
        Side(inArrow, true),
        Side(octagon, true),
    )
    p = Piece(
        Side(inArrow, false),
        Side(octagon, true),
        Side(outArrow, true),
        Side(outArrow, false),
    )
    puzz = emptyPuzzle()
    puzz[2, 2] = other
    @test fits(p, puzz, 1, 1)
    @test !fits(p, puzz, 2, 1)
    @test fits(p, puzz, 3, 1)
    @test !fits(p, puzz, 1, 2)
    @test !fits(p, puzz, 2, 2)
    @test fits(p, puzz, 3, 2)
    @test fits(p, puzz, 1, 3)
    @test !fits(p, puzz, 2, 3)
    @test fits(p, puzz, 3, 3)
end

function attachablePositions(puzz::Puzzle)
    placed = map(cti -> cti.I, findall(p -> !isnothing(p), puzz))
    isempty(placed) && return [(1, 1)]

    attachable = Tuple{Int,Int}[]

    for (i, j) in placed
        possible = [
            (i - 1, j)
            (i + 1, j)
            (i, j - 1)
            (i, j + 1)
        ]
        for p in possible
            (pi, pj) = p
            if pi < 1 || pi > 4 || pj < 1 || pj > 4
                continue
            end

            if !(p in placed)
                push!(attachable, p)
            end
        end
    end

    return unique(attachable)
end

@testset "attachablePositions" begin
    puzz = emptyPuzzle()
    @test attachablePositions(puzz) == [(1, 1)]

    p = Piece(
        Side(inArrow, true),
        Side(inArrow, true),
        Side(inArrow, true),
        Side(octagon, true),
    )
    puzz[1, 1] = p
    @test attachablePositions(puzz) == [(2, 1), (1, 2)]
    puzz[1, 2] = p
    @test attachablePositions(puzz) == [(2, 1), (2, 2), (1, 3)]
end

const solutionLock = ReentrantLock()

function solve(pieces::Vector{Piece}, puzz::Puzzle=emptyPuzzle())
    if isempty(pieces)
        lock(solutionLock)
        try 
            @debug "Found solution!"
            for ci in CartesianIndices(puzz)
                @debug "piece" ci puzz[ci]
            end
        finally
            unlock(solutionLock)
        end
        return [puzz]
    end

    lck = ReentrantLock()
    solutions = []
    possiblePositions = shuffle(attachablePositions(puzz))
    shuffledPieces = shuffle(pieces)

    Threads.@threads for (pi, p) in collect(enumerate(shuffledPieces))
        if length(possiblePositions) == 1 && length(pieces) > 1
            @debug "trying starting piece" p
        end
        Threads.@threads for cp in shuffle(combinations(p))
            for (i, j) in possiblePositions
                if fits(cp, puzz, i, j)
                    if length(pieces) < 2
                        left = get(puzz, i, j - 1)
                        top = get(puzz, i - 1, j)
                        right = get(puzz, i, j + 1)
                        bottom = get(puzz, i + 1, j)
                        @debug "getting closer" length(pieces) i j cp left top right bottom
                    end
                    newPieces = Base.copy(shuffledPieces)
                    deleteat!(newPieces, pi)
                    if length(newPieces) != (length(pieces) - 1)
                        error("didn't delete piece as expected")
                    end
                    newPuzz = Base.copy(puzz)
                    newPuzz[i, j] = cp
                    futureSolutions = solve(newPieces, newPuzz)
                    lock(lck)
                    try
                        append!(solutions, futureSolutions)
                    finally
                        unlock(lck)
                    end
                end
            end
        end
    end
    return solutions
end

@testset "solve" begin
    @test solve(Piece[]) == [emptyPuzzle()]
    pieces = [
        Piece(
            Side(inArrow, true),
            Side(inArrow, true),
            Side(inArrow, true),
            Side(octagon, true),
        ),
        Piece(
            Side(inArrow, false),
            Side(octagon, true),
            Side(outArrow, true),
            Side(outArrow, false),
        ),
    ]
    @test length(solve(pieces)) > 0
end

pieces = [
    Piece(Side(cross, false), Side(outArrow, true), Side(inArrow, true), Side(inArrow, false)),
    Piece(Side(outArrow, false), Side(outArrow, true), Side(octagon, true), Side(inArrow, false)),
    Piece(Side(outArrow, true), Side(octagon, true), Side(octagon, false), Side(outArrow, false)),
    Piece(Side(inArrow, false), Side(cross, true), Side(outArrow, true), Side(inArrow, false)),
    Piece(Side(octagon, false), Side(octagon, false), Side(octagon, true), Side(outArrow, true)),
    Piece(Side(cross, true), Side(octagon, true), Side(inArrow, false), Side(octagon, false)),
    Piece(Side(octagon, true), Side(octagon, false), Side(cross, false), Side(cross, true)),
    Piece(Side(outArrow, false), Side(cross, false), Side(octagon, true), Side(outArrow, true)),
    Piece(Side(outArrow, false), Side(cross, true), Side(octagon, true), Side(octagon, false)),
    Piece(Side(octagon, false), Side(inArrow, false), Side(cross, true), Side(inArrow, true)),
    Piece(Side(inArrow, false), Side(cross, true), Side(octagon, true), Side(inArrow, false)),
    Piece(Side(outArrow, true), Side(inArrow, true), Side(inArrow, false), Side(octagon, false)),
    Piece(Side(inArrow, true), Side(octagon, true), Side(outArrow, false), Side(outArrow, false)),
    Piece(Side(cross, false), Side(outArrow, false), Side(cross, true), Side(inArrow, true)),
    Piece(Side(cross, false), Side(octagon, false), Side(octagon, true), Side(inArrow, true)),
    Piece(Side(octagon, false), Side(cross, false), Side(outArrow, true), Side(outArrow, true)),
]
