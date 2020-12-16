module Day16 exposing (solution)

import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        ( rules, myTicket, nearbyTickets ) =
            parseInput input
    in
    nearbyTickets
        |> List.concatMap (\(Ticket values) -> values)
        |> List.filter (not << validForAny rules)
        |> List.sum
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


validTicket : (Int -> Bool) -> Ticket -> Bool
validTicket validationFunc (Ticket values) =
    values |> List.all validationFunc


validForAll : List Rule -> Int -> Bool
validForAll rules value =
    rules |> List.all (isValidFor value)


validForAny : List Rule -> Int -> Bool
validForAny rules value =
    rules |> List.any (isValidFor value)


isValidFor : Int -> Rule -> Bool
isValidFor value (Rule _ ( a, b ) ( c, d )) =
    (value >= a && value <= b) || (value >= c && value <= d)


type Rule
    = Rule String ( Int, Int ) ( Int, Int )


type Ticket
    = Ticket (List Int)



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
            ( [], Ticket [], [] )


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
            Rule name
                ( a |> (String.toInt >> Maybe.withDefault -1)
                , b |> (String.toInt >> Maybe.withDefault -1)
                )
                ( c |> (String.toInt >> Maybe.withDefault -1)
                , d |> (String.toInt >> Maybe.withDefault -1)
                )

        _ ->
            Rule "ERROR" ( -1, -1 ) ( -1, -1 )


parseYourTicket : String -> Ticket
parseYourTicket input =
    input
        |> String.lines
        |> List.drop 1
        |> List.map parseTicket
        |> List.head
        |> Maybe.withDefault (Ticket [])


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
        |> Ticket
