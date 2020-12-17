module Day17 exposing (solution)

import Dict exposing (Dict)
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInputWithDimension 3 input
        |> cycle offsets3
        |> cycle offsets3
        |> cycle offsets3
        |> cycle offsets3
        |> cycle offsets3
        |> cycle offsets3
        |> Set.size
        |> String.fromInt


part2 : Solver
part2 input =
    parseInputWithDimension 4 input
        |> cycle offsets4
        |> cycle offsets4
        |> cycle offsets4
        |> cycle offsets4
        |> cycle offsets4
        |> cycle offsets4
        |> Set.size
        |> String.fromInt


cycle : List Offset -> Set Cube -> Set Cube
cycle offsets activeCubes =
    let
        neighbourCount =
            countActiveNeighbours offsets activeCubes

        twoOrThree =
            neighbourCount |> Dict.filter (\k v -> v == 2 || v == 3) |> Dict.keys |> Set.fromList

        exactlyThree =
            neighbourCount |> Dict.filter (\k v -> v == 3) |> Dict.keys |> Set.fromList
    in
    activeCubes
        |> Set.intersect twoOrThree
        |> Set.union exactlyThree


countActiveNeighbours : List Offset -> Set Cube -> Dict Cube Int
countActiveNeighbours offsets activeCubes =
    activeCubes
        |> Set.foldl
            (\cube acc ->
                offsets
                    |> List.foldl
                        (\offset acc2 ->
                            acc2
                                |> Dict.update (cube |> plus offset)
                                    (\maybeValue ->
                                        case maybeValue of
                                            Nothing ->
                                                Just 1

                                            Just value ->
                                                Just (value + 1)
                                    )
                        )
                        acc
            )
            Dict.empty


plus : Cube -> Cube -> Cube
plus c1 c2 =
    List.map2 (\a b -> a + b) c1 c2


offsets4 : List Offset
offsets4 =
    [ 0, 0, 0 ]
        :: offsets3
        |> List.concatMap (\offset3 -> [ 0 :: offset3, 1 :: offset3, -1 :: offset3 ])
        |> List.filter ((/=) [ 0, 0, 0, 0 ])


offsets3 : List Offset
offsets3 =
    [ [ 1, 0, 0 ]
    , [ -1, 0, 0 ]
    , [ 0, 1, 0 ]
    , [ 1, 1, 0 ]
    , [ -1, 1, 0 ]
    , [ 0, -1, 0 ]
    , [ 1, -1, 0 ]
    , [ -1, -1, 0 ]
    , [ 0, 0, 1 ]
    , [ 1, 0, 1 ]
    , [ -1, 0, 1 ]
    , [ 0, 1, 1 ]
    , [ 1, 1, 1 ]
    , [ -1, 1, 1 ]
    , [ 0, -1, 1 ]
    , [ 1, -1, 1 ]
    , [ -1, -1, 1 ]
    , [ 0, 0, -1 ]
    , [ 1, 0, -1 ]
    , [ -1, 0, -1 ]
    , [ 0, 1, -1 ]
    , [ 1, 1, -1 ]
    , [ -1, 1, -1 ]
    , [ 0, -1, -1 ]
    , [ 1, -1, -1 ]
    , [ -1, -1, -1 ]
    ]



-- input parsing


type alias Cube =
    List Int


type alias Offset =
    List Int


parseInputWithDimension : Int -> String -> Set Cube
parseInputWithDimension dimension input =
    String.lines input
        |> List.indexedMap
            (\y line ->
                String.toList line
                    |> List.indexedMap
                        (\x ch ->
                            case ch of
                                '#' ->
                                    Just <| [ x, y ] ++ List.repeat (dimension - 2) 0

                                _ ->
                                    Nothing
                        )
            )
        |> List.concat
        |> List.filterMap identity
        |> Set.fromList
