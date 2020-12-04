module Day4 exposing (solution)

import Dict exposing (Dict)
import Html.Attributes exposing (required)
import Set
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> List.filter validPassport
        |> List.length
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


type alias Passport =
    Dict String String


validPassport : Passport -> Bool
validPassport pass =
    let
        required =
            Set.fromList [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

        fields =
            Set.fromList <| Dict.keys pass
    in
    Set.intersect fields required == required


parseInput : String -> List Passport
parseInput input =
    input
        |> String.split "\n\n"
        |> List.map parseLine


parseLine : String -> Passport
parseLine input =
    input
        |> String.replace "\n" " "
        |> String.split " "
        |> List.map parseField
        |> Dict.fromList


parseField : String -> ( String, String )
parseField field =
    case String.split ":" field of
        [ a, b ] ->
            ( a, b )

        _ ->
            ( "invalid", "invalid" )
