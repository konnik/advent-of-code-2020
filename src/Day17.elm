module Day17 exposing (countActiveNeighbours, offsets, parseInput, solution, step, test)

import Dict exposing (Dict)
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInput input
        |> step
        |> step
        |> step
        |> step
        |> step
        |> step
        |> Set.size
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


test : String
test =
    """.#.
..#
###"""


step : Set ( Int, Int, Int ) -> Set ( Int, Int, Int )
step activeCubes =
    let
        neighbourCount =
            countActiveNeighbours activeCubes

        twoOrThree : Set ( Int, Int, Int )
        twoOrThree =
            neighbourCount |> Dict.filter (\k v -> v == 2 || v == 3) |> Dict.keys |> Set.fromList

        exactlyThree : Set ( Int, Int, Int )
        exactlyThree =
            neighbourCount |> Dict.filter (\k v -> v == 3) |> Dict.keys |> Set.fromList
    in
    activeCubes
        |> Set.intersect twoOrThree
        |> Set.union exactlyThree


countActiveNeighbours : Set ( Int, Int, Int ) -> Dict ( Int, Int, Int ) Int
countActiveNeighbours activeCubes =
    activeCubes
        |> Set.foldl
            (\( x, y, z ) acc ->
                offsets
                    |> List.foldl
                        (\( dx, dy, dz ) acc2 ->
                            acc2
                                |> Dict.update ( x + dx, y + dy, z + dz )
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


offsets : List ( Int, Int, Int )
offsets =
    [ ( 1, 0, 0 )
    , ( -1, 0, 0 )
    , ( 0, 1, 0 )
    , ( 1, 1, 0 )
    , ( -1, 1, 0 )
    , ( 0, -1, 0 )
    , ( 1, -1, 0 )
    , ( -1, -1, 0 )
    , ( 0, 0, 1 )
    , ( 1, 0, 1 )
    , ( -1, 0, 1 )
    , ( 0, 1, 1 )
    , ( 1, 1, 1 )
    , ( -1, 1, 1 )
    , ( 0, -1, 1 )
    , ( 1, -1, 1 )
    , ( -1, -1, 1 )
    , ( 0, 0, -1 )
    , ( 1, 0, -1 )
    , ( -1, 0, -1 )
    , ( 0, 1, -1 )
    , ( 1, 1, -1 )
    , ( -1, 1, -1 )
    , ( 0, -1, -1 )
    , ( 1, -1, -1 )
    , ( -1, -1, -1 )
    ]



-- input parsing


parseInput : String -> Set ( Int, Int, Int )
parseInput input =
    String.lines input
        |> List.indexedMap
            (\y line ->
                String.toList line
                    |> List.indexedMap
                        (\x ch ->
                            case ch of
                                '#' ->
                                    Just ( x, y, 0 )

                                _ ->
                                    Nothing
                        )
            )
        |> List.concat
        |> List.filterMap identity
        |> Set.fromList
