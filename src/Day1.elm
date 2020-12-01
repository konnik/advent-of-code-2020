module Day1 exposing (solution)

import Set
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        numbers =
            input
                |> String.words
                |> List.map (String.toInt >> Maybe.withDefault 0)
    in
    numbers
        |> allPairs
        |> List.filter (\( a, b ) -> a + b == 2020)
        |> List.map (\( a, b ) -> a * b)
        |> List.map String.fromInt
        |> List.head
        |> Maybe.withDefault "Kunde inte hittqa något svar."


allPairs : List Int -> List ( Int, Int )
allPairs numbers =
    case numbers of
        [] ->
            []

        n :: [] ->
            [ ( n, n ) ]

        n :: rest ->
            let
                pairsWithN =
                    rest |> List.map (Tuple.pair n)
            in
            pairsWithN ++ allPairs rest


part2 : Solver
part2 input =
    "not implemented"
