module Day9 exposing (solution)

import Set
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
    "not implemented"


preambleLength : Int
preambleLength =
    25


findFirstInvalid : List Int -> Maybe Int
findFirstInvalid numbers =
    let
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
