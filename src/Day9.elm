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

        difference =
            numbers |> findFirstInvalid |> Maybe.withDefault -1

        partialSums =
            numbers
                |> calcPartialSums

        startSum =
            findPairWithDifference difference partialSums

        values =
            partialSums
                |> List.filter (\( sum, num ) -> sum > startSum && sum <= startSum + difference)
                |> List.map Tuple.second
    in
    values
        |> sumMinAndMax
        |> String.fromInt


sumMinAndMax : List Int -> Int
sumMinAndMax values =
    [ List.minimum values
    , List.maximum values
    ]
        |> List.filterMap identity
        |> List.sum


findPairWithDifference : Int -> List ( Int, Int ) -> Int
findPairWithDifference difference partialSums2 =
    let
        sums =
            partialSums2 |> List.map Tuple.first |> Set.fromList
    in
    sums
        |> Set.filter (\a -> Set.member (a + difference) sums)
        |> Set.toList
        |> List.head
        |> Maybe.withDefault -1


pairFromList : List Int -> Maybe ( Int, Int )
pairFromList numbers =
    case numbers of
        [ a, b ] ->
            Just ( a, b )

        _ ->
            Nothing


calcPartialSums : List Int -> List ( Int, Int )
calcPartialSums numbers =
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
