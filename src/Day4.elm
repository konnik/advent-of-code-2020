module Day4 exposing (solution)

import Dict exposing (Dict)
import Html.Attributes exposing (required)
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    input
        |> parseInput
        |> List.filter validPassport1
        |> List.length
        |> String.fromInt


part2 : Solver
part2 input =
    input
        |> parseInput
        |> List.filter validPassport2
        |> List.length
        |> String.fromInt


type alias Passport =
    Dict String String


validPassport1 : Passport -> Bool
validPassport1 pass =
    let
        required =
            Set.fromList [ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" ]

        fields =
            Set.fromList <| Dict.keys pass
    in
    Set.intersect fields required == required


validPassport2 : Passport -> Bool
validPassport2 pass =
    let
        required : List ( String, String -> Bool )
        required =
            [ ( "byr", byr ), ( "iyr", iyr ), ( "eyr", eyr ), ( "hgt", hgt ), ( "hcl", hcl ), ( "ecl", ecl ), ( "pid", pid ) ]

        validate : ( String, String -> Bool ) -> Bool
        validate ( field, predicate ) =
            Dict.get field pass
                |> Maybe.map predicate
                |> Maybe.withDefault False
    in
    required
        |> List.map validate
        |> List.all identity


{-| byr (Birth Year) - four digits; at least 1920 and at most 2002.
-}
byr : String -> Bool
byr =
    allOf [ digits 4, atLeast 1920, atMost 2002 ]


{-| iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-}
iyr : String -> Bool
iyr =
    allOf [ digits 4, atLeast 2010, atMost 2020 ]


{-| eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-}
eyr : String -> Bool
eyr =
    allOf [ digits 4, atLeast 2020, atMost 2030 ]


{-| hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
-}
hgt : String -> Bool
hgt x =
    case ( String.dropRight 2 x, String.right 2 x ) of
        ( value, "cm" ) ->
            value |> allOf [ digits 3, atLeast 150, atMost 193 ]

        ( value, "in" ) ->
            value |> allOf [ digits 2, atLeast 59, atMost 76 ]

        _ ->
            False


{-| hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-}
hcl : String -> Bool
hcl x =
    case ( String.left 1 x, String.dropLeft 1 x ) of
        ( "#", value ) ->
            hexDigits 6 value

        _ ->
            False


{-| ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-}
ecl : String -> Bool
ecl x =
    x |> choice [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]


{-| pid (Passport ID) - a nine-digit number, including leading zeroes.
-}
pid : String -> Bool
pid x =
    x |> digits 9



-- some validation primitives


allOf : List (String -> Bool) -> String -> Bool
allOf predicates str =
    predicates |> List.map (\p -> p str) |> List.all identity


choice : List String -> String -> Bool
choice choices str =
    List.member str choices


digits : Int -> String -> Bool
digits n str =
    String.length str == n && String.all Char.isDigit str


hexDigit : Char -> Bool
hexDigit ch =
    List.member ch (String.toList "0123456789abcdef")


hexDigits : Int -> String -> Bool
hexDigits n str =
    String.length str == n && String.all hexDigit str


atLeast : Int -> String -> Bool
atLeast value str =
    str
        |> String.toInt
        |> Maybe.map (\x -> x >= value)
        |> Maybe.withDefault False


atMost : Int -> String -> Bool
atMost value str =
    str
        |> String.toInt
        |> Maybe.map (\x -> x <= value)
        |> Maybe.withDefault False



-- parse input


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
