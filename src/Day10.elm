module Day10 exposing (solution)

import Html exposing (input)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> List.sort
        |> List.foldl
            (\curr ( prev, diff1, diff3 ) ->
                if curr - prev == 1 then
                    ( curr, diff1 + 1, diff3 )

                else if curr - prev == 3 then
                    ( curr, diff1, diff3 + 1 )

                else
                    ( curr, diff1, diff3 )
            )
            ( 0, 0, 0 )
        |> (\( _, diff1, diff3 ) -> diff1 * (diff3 + 1))
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"



-- parse input


parseInput : String -> List Int
parseInput input =
    String.lines input |> List.map (String.toInt >> Maybe.withDefault -1)
