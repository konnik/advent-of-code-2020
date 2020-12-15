module Day15 exposing (solution)

import Dict exposing (Dict)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        startingNumbers =
            input
                |> String.split ","
                |> List.map (String.toInt >> Maybe.withDefault 0)

        recentlySpokenNumber =
            startingNumbers |> List.reverse |> List.head |> Maybe.withDefault 0
    in
    startingNumbers
        |> List.indexedMap (\i n -> ( n, i + 1 ))
        |> Dict.fromList
        |> speak 2020 (List.length startingNumbers) recentlySpokenNumber Nothing
        |> String.fromInt


part2 : Solver
part2 input =
    let
        startingNumbers =
            input
                |> String.split ","
                |> List.map (String.toInt >> Maybe.withDefault 0)

        recentlySpokenNumber =
            startingNumbers |> List.reverse |> List.head |> Maybe.withDefault 0
    in
    startingNumbers
        |> List.indexedMap (\i n -> ( n, i + 1 ))
        |> Dict.fromList
        |> speak 30000000 (List.length startingNumbers) recentlySpokenNumber Nothing
        |> String.fromInt


speak : Int -> Int -> Int -> Maybe Int -> Dict Int Int -> Int
speak target turn spokenNumber spokenBefore state =
    if turn == target then
        spokenNumber

    else
        case spokenBefore of
            Nothing ->
                speak target (turn + 1) 0 (Dict.get 0 state) (Dict.insert 0 (turn + 1) state)

            Just turnBefore ->
                speak target (turn + 1) (turn - turnBefore) (Dict.get (turn - turnBefore) state) (Dict.insert (turn - turnBefore) (turn + 1) state)
