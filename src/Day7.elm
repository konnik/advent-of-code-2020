module Day7 exposing (solution)

import Dict exposing (Dict)
import Result.Extra as RE
import Set
import String
import Types exposing (Solution, Solver)
import Url.Parser exposing (parse)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        rules =
            parseInput input
    in
    Dict.keys rules
        |> List.filter (canHoldShinyGold rules)
        |> List.length
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


type alias Color =
    String


canHoldShinyGold : Dict Color (List ( Int, Color )) -> String -> Bool
canHoldShinyGold rules color =
    let
        colors =
            Dict.get color rules |> Maybe.map (List.map Tuple.second) |> Maybe.withDefault [] |> Set.fromList
    in
    Set.member "shiny gold" colors
        || (Set.toList colors |> List.any (\c -> canHoldShinyGold rules c))


parseInput : String -> Dict Color (List ( Int, Color ))
parseInput input =
    input
        |> String.lines
        |> List.map parseLine
        |> Dict.fromList


parseLine : String -> ( Color, List ( Int, Color ) )
parseLine line =
    let
        --dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        -- =>> dark olive 3 faded blue bags 4 dotted black bags
        normalizedLine =
            line
                |> String.replace "," ""
                |> String.replace "." ""
                |> String.replace "bags contain " ""
                |> String.replace "bags" "bag"
    in
    case String.words normalizedLine of
        color1 :: color2 :: rest ->
            ( color1 ++ " " ++ color2, parseContains rest )

        _ ->
            ( "error", [] )


parseContains : List String -> List ( Int, Color )
parseContains words =
    case words of
        numStr :: color1 :: color2 :: "bag" :: rest ->
            let
                num =
                    String.toInt numStr |> Maybe.withDefault -1

                color =
                    color1 ++ " " ++ color2
            in
            ( num, color ) :: parseContains rest

        _ ->
            []
