module Day11 exposing (solution)

import Dict exposing (Dict)
import List
import Maybe
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInput input
        |> stabilize
        |> Dict.values
        |> List.filter (\cell -> cell == Occupied)
        |> List.length
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"



-- solve


stabilize : Grid -> Grid
stabilize grid =
    let
        nextGrid =
            step grid
    in
    if grid == nextGrid then
        nextGrid

    else
        stabilize nextGrid


occupiedNeighbours : ( Int, Int ) -> Dict ( Int, Int ) Cell -> Int
occupiedNeighbours ( x, y ) cells =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]
        |> List.filterMap
            (\( dx, dy ) ->
                Dict.get ( x + dx, y + dy ) cells
            )
        |> List.filter (\c -> c == Occupied)
        |> List.length


step : Grid -> Grid
step grid =
    let
        newFGrid =
            Dict.toList grid
                |> List.foldl
                    (\( ( x, y ), cell ) newCells ->
                        let
                            newCell =
                                case ( cell, occupiedNeighbours ( x, y ) grid ) of
                                    ( Empty, 0 ) ->
                                        Occupied

                                    ( Occupied, n ) ->
                                        if n >= 4 then
                                            Empty

                                        else
                                            cell

                                    _ ->
                                        cell
                        in
                        Dict.insert ( x, y ) newCell newCells
                    )
                    Dict.empty
    in
    newFGrid



-- parse


type alias Grid =
    Dict ( Int, Int ) Cell


type Cell
    = Floor
    | Empty
    | Occupied


parseInput : String -> Grid
parseInput input =
    let
        cells =
            input
                |> String.lines
                |> List.indexedMap
                    (\y line ->
                        String.toList line
                            |> List.indexedMap
                                (\x ch ->
                                    case ch of
                                        '.' ->
                                            ( ( x + 1, y + 1 ), Floor )

                                        'L' ->
                                            ( ( x + 1, y + 1 ), Empty )

                                        _ ->
                                            ( ( x + 1, y + 1 ), Occupied )
                                )
                    )
                |> List.concat
                |> Dict.fromList
    in
    cells
