module Day2 exposing (solution)

import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> String.lines
        |> List.map parseLine
        |> List.filter validPassword1
        |> List.length
        |> String.fromInt


part2 : Solver
part2 input =
    input
        |> String.lines
        |> List.map parseLine
        |> List.filter validPassword2
        |> List.length
        |> String.fromInt


type Password
    = Password Int Int Char (List Char)


validPassword1 : Password -> Bool
validPassword1 (Password min max char password) =
    let
        count =
            password
                |> List.filter ((==) char)
                |> List.length
    in
    count >= min && count <= max


validPassword2 : Password -> Bool
validPassword2 (Password a b char password) =
    let
        charA =
            password |> List.drop (a - 1) |> List.head |> Maybe.withDefault '.'

        charB =
            password |> List.drop (b - 1) |> List.head |> Maybe.withDefault '.'
    in
    (charA == char && charB /= char) || (charA /= char && charB == char)


parseLine : String -> Password
parseLine line =
    let
        normalizedLine =
            line
                |> String.replace "-" " "
                |> String.replace ":" ""
    in
    case String.split " " normalizedLine of
        [ a, b, c, d ] ->
            Password
                (String.toInt a |> Maybe.withDefault -1)
                (String.toInt b |> Maybe.withDefault -1)
                (String.uncons c |> Maybe.map Tuple.first |> Maybe.withDefault '.')
                (String.toList d)

        _ ->
            Password -1 -1 '.' []
