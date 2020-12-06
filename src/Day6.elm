module Day6 exposing (solution)

import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> List.map countAnyone
        |> List.sum
        |> String.fromInt


part2 : Solver
part2 input =
    input
        |> parseInput
        |> List.map countEveryone
        |> List.sum
        |> String.fromInt


type alias GroupAnswers =
    List (Set Char)


countAnyone : GroupAnswers -> Int
countAnyone answers =
    answers
        |> foldlWithDefault Set.empty Set.union
        |> Set.size


countEveryone : GroupAnswers -> Int
countEveryone answers =
    answers
        |> foldlWithDefault Set.empty Set.intersect
        |> Set.size


foldlWithDefault : a -> (a -> a -> a) -> List a -> a
foldlWithDefault default f items =
    case items of
        [] ->
            default

        x :: xs ->
            List.foldl f x xs



-- parse input


parseInput : String -> List GroupAnswers
parseInput input =
    input
        |> String.split "\n\n"
        |> List.map parseGroup


parseGroup : String -> GroupAnswers
parseGroup group =
    group
        |> String.split "\n"
        |> List.map String.toList
        |> List.map Set.fromList
