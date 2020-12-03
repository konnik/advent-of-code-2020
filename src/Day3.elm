module Day3 exposing (solution)

import Dict exposing (Dict)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> countTrees
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


countTrees : Grid -> Int
countTrees grid =
    let
        cellValue : Cell -> Int
        cellValue cell =
            if cell == Tree then
                1

            else
                0

        count : Dict Int Cell -> ( Int, Int ) -> ( Int, Int )
        count rad ( pos, acc ) =
            ( pos + 3, acc + (Dict.get (pos |> modBy (Dict.size rad)) rad |> Maybe.withDefault Tree |> cellValue) )
    in
    grid
        |> List.foldl count ( 0, 0 )
        |> Tuple.second



-- parsing


type alias Grid =
    List (Dict Int Cell)


type Cell
    = Open
    | Tree


parseInput : String -> Grid
parseInput =
    String.lines >> List.map parseLine


parseLine : String -> Dict Int Cell
parseLine =
    String.toList >> List.indexedMap (\i c -> ( i, toCell c )) >> Dict.fromList


toCell : Char -> Cell
toCell ch =
    case ch of
        '#' ->
            Tree

        _ ->
            Open
