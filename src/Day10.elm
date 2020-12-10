module Day10 exposing (solution)

import Element.Font exposing (tabularNumbers)
import Html exposing (input)
import List
import List.Extra as LE
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> List.sort
        |> countDeltas
        |> (\( diff1, _, diff3 ) -> diff1 * (diff3 + 1))
        |> String.fromInt


part2 : Solver
part2 input =
    parseInput input
        |> (::) 0
        |> List.sort
        |> group
        |> List.map validCombinations
        |> List.product
        |> String.fromInt


countDeltas : List Int -> ( Int, Int, Int )
countDeltas numbers =
    numbers
        |> List.foldl
            (\curr ( prev, ( diff1, diff2, diff3 ) ) ->
                case curr - prev of
                    1 ->
                        ( curr, ( diff1 + 1, diff2, diff3 ) )

                    2 ->
                        ( curr, ( diff1, diff2 + 1, diff3 ) )

                    3 ->
                        ( curr, ( diff1, diff2, diff3 + 1 ) )

                    _ ->
                        ( curr, ( diff1, diff2, diff3 ) )
            )
            ( 0, ( 0, 0, 0 ) )
        |> Tuple.second


group : List Int -> List ( Int, List Int )
group numbers =
    numbers
        |> LE.groupWhile (\a b -> b - a < 3)


validCombinations : ( Int, List Int ) -> Int
validCombinations numbers =
    case numbers of
        ( _, [] ) ->
            1

        ( _, [ _ ] ) ->
            1

        ( _, a :: [ _ ] ) ->
            2

        ( _, a :: b :: [ _ ] ) ->
            4

        ( _, a :: b :: c :: rest ) ->
            validCombinations ( a, b :: c :: rest )
                + validCombinations ( b, c :: rest )
                + validCombinations ( c, rest )



-- parse input


parseInput : String -> List Int
parseInput input =
    String.lines input |> List.map (String.toInt >> Maybe.withDefault -1)
