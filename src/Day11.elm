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
        |> stabilize (step 4 countOccupiedNeigbourSeats)
        |> countOccupiedSeats
        |> String.fromInt


part2 : Solver
part2 input =
    parseInput input
        |> stabilize (step 5 countVisibleOccupiedSeats)
        |> countOccupiedSeats
        |> String.fromInt



-- solve


type Cell
    = Floor
    | Empty
    | Occupied


type alias Pos =
    ( Int, Int )


type alias Dir =
    ( Int, Int )


type alias Grid =
    Dict Pos Cell


countOccupiedSeats : Grid -> Int
countOccupiedSeats grid =
    grid
        |> Dict.values
        |> List.filter (\cell -> cell == Occupied)
        |> List.length


stabilize : (Grid -> Grid) -> Grid -> Grid
stabilize stepFunc grid =
    let
        nextGrid =
            stepFunc grid
    in
    if grid == nextGrid then
        nextGrid

    else
        stabilize stepFunc nextGrid


countOccupiedNeigbourSeats : Pos -> Grid -> Int
countOccupiedNeigbourSeats ( x, y ) grid =
    directions
        |> List.map (isNeighbourOccupied ( x, y ) grid)
        |> List.filter identity
        |> List.length


isNeighbourOccupied : Pos -> Grid -> Dir -> Bool
isNeighbourOccupied ( x, y ) grid ( dx, dy ) =
    case Dict.get ( x + dx, y + dy ) grid of
        Just Occupied ->
            True

        _ ->
            False


directions : List Dir
directions =
    [ ( -1, -1 ), ( -1, 0 ), ( -1, 1 ), ( 0, -1 ), ( 0, 1 ), ( 1, -1 ), ( 1, 0 ), ( 1, 1 ) ]


countVisibleOccupiedSeats : Pos -> Grid -> Int
countVisibleOccupiedSeats ( x, y ) grid =
    directions
        |> List.map (canSeeOccupied ( x, y ) grid)
        |> List.filter identity
        |> List.length


canSeeOccupied : Pos -> Grid -> Dir -> Bool
canSeeOccupied ( x, y ) grid ( dx, dy ) =
    case Dict.get ( x + dx, y + dy ) grid of
        Just Occupied ->
            True

        Just Empty ->
            False

        Just Floor ->
            canSeeOccupied ( x + dx, y + dy ) grid ( dx, dy )

        Nothing ->
            False


step : Int -> (Pos -> Grid -> Int) -> Grid -> Grid
step tolerance countOccupiedFunc grid =
    Dict.toList grid
        |> List.foldl
            (\( ( x, y ), cell ) newGrid ->
                case ( cell, countOccupiedFunc ( x, y ) grid ) of
                    ( Empty, 0 ) ->
                        Dict.insert ( x, y ) Occupied newGrid

                    ( Occupied, n ) ->
                        if n >= tolerance then
                            Dict.insert ( x, y ) Empty newGrid

                        else
                            newGrid

                    _ ->
                        newGrid
            )
            grid



-- parse


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
