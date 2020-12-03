module Day3 exposing (solution)

import Dict exposing (Dict)
import String
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        grid =
            parseInput input
    in
    Slope 3 1
        |> countTrees grid
        |> String.fromInt


part2 : Solver
part2 input =
    let
        grid =
            parseInput input
    in
    [ Slope 1 1
    , Slope 3 1
    , Slope 5 1
    , Slope 7 1
    , Slope 1 2
    ]
        |> List.map (countTrees grid)
        |> List.foldl (*) 1
        |> String.fromInt


type Slope
    = Slope Int Int


type Pos
    = Pos Int Int


type Cell
    = Open
    | Tree


type alias Grid =
    List (Dict Int Cell)


countTrees : Grid -> Slope -> Int
countTrees grid (Slope stepRight stepDown) =
    let
        countRow : Dict Int Cell -> ( Pos, Int ) -> ( Pos, Int )
        countRow gridRow ( Pos col row, acc ) =
            let
                cell =
                    gridRow
                        |> Dict.get (col |> modBy (Dict.size gridRow))
                        |> Maybe.withDefault Open
            in
            if (row |> modBy stepDown) == 0 then
                ( Pos (col + stepRight) (row + 1), acc + cellValue cell )

            else
                ( Pos col (row + 1), acc )
    in
    grid
        |> List.foldl countRow ( Pos 0 0, 0 )
        |> Tuple.second


cellValue : Cell -> Int
cellValue cell =
    if cell == Tree then
        1

    else
        0



-- parsing of input


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
