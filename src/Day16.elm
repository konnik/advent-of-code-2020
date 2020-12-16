module Day16 exposing (solution)

import Dict exposing (Dict, values)
import Korv exposing (rules, yourTicket)
import List
import List.Extra as LE
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        ( rules, _, nearbyTickets ) =
            parseInput input
    in
    nearbyTickets
        |> List.concat
        |> List.filter (not << validForAny rules)
        |> List.sum
        |> String.fromInt


part2 : Solver
part2 input =
    let
        ( rules, yourTicket, nearbyTickets ) =
            parseInput input

        validTickets : List (List Int)
        validTickets =
            nearbyTickets
                |> List.filter (validTicket rules)
    in
    validTickets
        |> asColumns
        |> List.map (possibleFields rules)
        |> List.indexedMap Tuple.pair
        |> List.sortBy numberOfPossibleFields
        |> resolveFields
        |> List.filter (fieldStartsWith "departure")
        |> List.map (posToValueIn yourTicket)
        |> List.product
        |> String.fromInt


type alias Rule =
    ( String, ( Int, Int ), ( Int, Int ) )


type alias Ticket =
    List Int


resolveFields : List ( Int, List String ) -> List ( Int, String )
resolveFields columns =
    case columns of
        [] ->
            []

        ( col, [ resolvedField ] ) :: rest ->
            ( col, resolvedField ) :: resolveFields (rest |> withoutField resolvedField)

        _ ->
            [ ( -1, "Error" ) ]


withoutField : String -> List ( Int, List String ) -> List ( Int, List String )
withoutField fieldToRemove columns =
    columns
        |> List.map
            (\( col, fields ) ->
                ( col, fields |> List.filter (\f -> f /= fieldToRemove) )
            )
        |> List.sortBy numberOfPossibleFields


asColumns : List (List Int) -> List (List Int)
asColumns =
    LE.transpose


numberOfPossibleFields : ( Int, List String ) -> Int
numberOfPossibleFields =
    Tuple.second >> List.length


fieldStartsWith : String -> ( Int, String ) -> Bool
fieldStartsWith str ( _, name ) =
    name |> String.startsWith str


posToValueIn : Ticket -> ( Int, a ) -> Int
posToValueIn ticket ( col, _ ) =
    ticket |> List.drop col |> List.head |> Maybe.withDefault -1


possibleFields : List Rule -> List Int -> List String
possibleFields rules column =
    let
        allFields : List String
        allFields =
            rules |> List.map fieldname
    in
    column
        |> List.foldl
            (\v remaining ->
                remaining
                    |> List.filter (\f -> List.member f (validFields rules v))
            )
            allFields


validFields : List Rule -> Int -> List String
validFields rules v =
    rules
        |> List.filter
            (\( _, ( a, b ), ( c, d ) ) ->
                (a <= v && v <= b) || (c <= v && v <= d)
            )
        |> List.map fieldname


fieldname : ( String, a, a ) -> String
fieldname ( n, _, _ ) =
    n


validTicket : List Rule -> List Int -> Bool
validTicket rules ticket =
    ticket
        |> List.all
            (\v ->
                rules
                    |> List.any (isValidFor v)
            )


validForAny : List Rule -> Int -> Bool
validForAny rules value =
    rules |> List.any (isValidFor value)


isValidFor : Int -> Rule -> Bool
isValidFor v ( _, ( a, b ), ( c, d ) ) =
    (a <= v && v <= b) || (c <= v && v <= d)



-- input parsing


parseInput : String -> ( List Rule, Ticket, List Ticket )
parseInput input =
    case input |> String.split "\n\n" of
        [ a, b, c ] ->
            ( a |> parseRules
            , b |> parseYourTicket
            , c |> parseNearbyTickets
            )

        _ ->
            ( [], [], [] )


parseRules : String -> List Rule
parseRules input =
    input
        |> String.lines
        |> List.map parseRule


parseRule : String -> Rule
parseRule input =
    let
        normalizedInput =
            input
                |> String.replace ": " "|"
                |> String.replace "-" "|"
                |> String.replace " or " "|"
    in
    case String.split "|" normalizedInput of
        [ name, a, b, c, d ] ->
            ( name
            , ( parseInt a, parseInt b )
            , ( parseInt c, parseInt d )
            )

        _ ->
            ( "ERROR", ( -1, -1 ), ( -1, -1 ) )


parseInt : String -> Int
parseInt =
    String.toInt >> Maybe.withDefault -1


parseYourTicket : String -> Ticket
parseYourTicket input =
    input
        |> String.lines
        |> List.drop 1
        |> List.map parseTicket
        |> List.head
        |> Maybe.withDefault []


parseNearbyTickets : String -> List Ticket
parseNearbyTickets input =
    input
        |> String.lines
        |> List.drop 1
        |> List.map parseTicket


parseTicket : String -> Ticket
parseTicket input =
    input
        |> String.split ","
        |> List.filterMap String.toInt
