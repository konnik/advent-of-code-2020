module Day5 exposing (solution)

import Dict
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> (List.maximum >> Maybe.withDefault -1)
        |> String.fromInt


part2 : Solver
part2 input =
    let
        seatIds : Set Int
        seatIds =
            parseInput input |> Set.fromList

        emptyWithNeightbours : Int -> Bool
        emptyWithNeightbours seatId =
            not (Set.member seatId seatIds)
                && Set.member (seatId - 1) seatIds
                && Set.member (seatId + 1) seatIds
    in
    List.range 0 1023
        |> List.filter emptyWithNeightbours
        |> (List.head >> Maybe.withDefault -1)
        |> String.fromInt



-- input parsing


parseInput : String -> List Int
parseInput input =
    String.lines input
        |> List.map toSeatId


toSeatId : String -> Int
toSeatId boardingPass =
    boardingPass
        |> String.toList
        |> List.map charToBit
        |> List.foldl (\bit acc -> acc * 2 + bit) 0


charToBit : Char -> Int
charToBit ch =
    Dict.get ch (Dict.fromList [ ( 'B', 1 ), ( 'R', 1 ) ]) |> Maybe.withDefault 0
