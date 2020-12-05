module Day5 exposing (solution)

import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    String.lines input
        |> List.map seatId
        |> List.maximum
        |> Maybe.withDefault -1
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


seatId : String -> Int
seatId pass =
    let
        charToBit ch =
            if ch == 'B' || ch == 'R' then
                1

            else
                0
    in
    pass
        |> String.toList
        |> List.map charToBit
        |> List.foldl (\bit acc -> acc * 2 + bit) 0
