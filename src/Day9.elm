module Day9 exposing (solution)

import List.Extra as LE
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> findFirstInvalid
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "no invalid number found"


part2 : Solver
part2 input =
    let
        numbers =
            parseInput input

        invalidNumber =
            numbers |> findFirstInvalid |> Maybe.withDefault -1

        sums =
            numbers
                |> partialSums

        pairsOfSums : List ( Int, Int )
        pairsOfSums =
            sums |> List.map Tuple.first |> allPairs

        partialSumsWithDifference =
            pairsOfSums
                |> List.filter (\( a, b ) -> b - a == invalidNumber)
                |> List.head
    in
    case partialSumsWithDifference of
        Just ( a, b ) ->
            let
                valuesBeweenPartialSums =
                    sums |> List.filter (\( sum, num ) -> sum > a && sum <= b) |> List.map Tuple.second

                minValue =
                    List.minimum valuesBeweenPartialSums |> Maybe.withDefault -1

                maxValue =
                    List.maximum valuesBeweenPartialSums |> Maybe.withDefault -1
            in
            minValue + maxValue |> String.fromInt

        Nothing ->
            "could not find answer"


partialSums : List Int -> List ( Int, Int )
partialSums numbers =
    numbers
        |> LE.scanl (\num ( prevSum, prevNum ) -> ( prevSum + num, num )) ( 0, 0 )


findFirstInvalid : List Int -> Maybe Int
findFirstInvalid numbers =
    let
        preambleLength : Int
        preambleLength =
            25

        preamble =
            List.take preambleLength numbers

        preambleSums =
            preamble |> allPairs |> List.map (\( a, b ) -> a + b) |> Set.fromList
    in
    case List.drop preambleLength numbers of
        [] ->
            Nothing

        a :: rest ->
            if Set.member a preambleSums then
                case numbers of
                    _ :: tail ->
                        findFirstInvalid tail

                    _ ->
                        Nothing

            else
                Just a


allPairs : List Int -> List ( Int, Int )
allPairs numbers =
    case numbers of
        [] ->
            []

        a :: rest ->
            let
                pairsWithA =
                    rest |> List.map (Tuple.pair a)
            in
            pairsWithA ++ allPairs rest



-- parse input


parseInput : String -> List Int
parseInput input =
    String.lines input
        |> List.filterMap String.toInt
