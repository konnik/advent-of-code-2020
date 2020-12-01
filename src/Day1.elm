module Day1 exposing (solution)

import Set
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInput input
        |> allPairs
        |> List.filter (withPairSum 2020)
        |> List.map toPairProduct
        |> List.head
        |> answerToString


part2 : Solver
part2 input =
    parseInput input
        |> allTriplets
        |> List.filter (withTripletSum 2020)
        |> List.map toTripletProduct
        |> List.head
        |> answerToString


answerToString : Maybe Int -> String
answerToString maybeAnswer =
    maybeAnswer
        |> Maybe.map String.fromInt
        |> Maybe.withDefault "Kunde inte hitta något svar."


withPairSum : Int -> ( Int, Int ) -> Bool
withPairSum sum ( a, b ) =
    a + b == sum


withTripletSum : Int -> ( Int, Int, Int ) -> Bool
withTripletSum sum ( a, b, c ) =
    sum == a + b + c


toPairProduct : ( Int, Int ) -> Int
toPairProduct ( a, b ) =
    a * b


toTripletProduct : ( Int, Int, Int ) -> Int
toTripletProduct ( a, b, c ) =
    a * b * c


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


allTriplets : List Int -> List ( Int, Int, Int )
allTriplets numbers =
    case numbers of
        [] ->
            []

        a :: rest ->
            let
                tripletsWithA =
                    allPairs rest |> List.map (\( b, c ) -> ( a, b, c ))
            in
            tripletsWithA ++ allTriplets rest


parseInput : String -> List Int
parseInput input =
    input
        |> String.words
        |> List.map (String.toInt >> Maybe.withDefault 0)
