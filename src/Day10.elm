module Day10 exposing (solution)

import Dict exposing (Dict)
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
    let
        adapters =
            parseInput input

        max =
            List.maximum adapters |> Maybe.withDefault 0
    in
    [ [ 0 ], adapters, [ max + 3 ] ]
        |> (List.concat >> List.sort >> List.reverse)
        |> solve2 Dict.empty
        |> (Tuple.second >> String.fromInt)


solve2 : Dict (List Int) Int -> List Int -> ( Dict (List Int) Int, Int )
solve2 memo adapters =
    case Dict.get adapters memo of
        Just x ->
            ( memo, x )

        Nothing ->
            case adapters of
                [] ->
                    ( Dict.insert [] 0 memo, 0 )

                [ _ ] ->
                    ( Dict.insert [] 1 memo, 1 )

                x :: rest ->
                    let
                        ( newMemo, value ) =
                            rest
                                |> LE.takeWhile (\n -> x - n <= 3)
                                |> List.foldl
                                    (\n ( accMemo, accValue ) ->
                                        let
                                            ( mm, vv ) =
                                                solve2 accMemo <| LE.dropWhile ((/=) n) rest
                                        in
                                        ( mm, accValue + vv )
                                    )
                                    ( memo, 0 )
                    in
                    ( Dict.insert adapters value newMemo, value )


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



-- parse input


parseInput : String -> List Int
parseInput input =
    String.lines input |> List.map (String.toInt >> Maybe.withDefault -1)
