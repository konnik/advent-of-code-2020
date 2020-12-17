module Day12 exposing (solution)

import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInput input
        |> List.foldl step initialState
        |> manhattanDist
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


type alias State =
    { x : Int, y : Int, dir : Int }


type Action
    = North Int
    | South Int
    | East Int
    | West Int
    | TurnLeft Int
    | TurnRight Int
    | Forward Int


initialState : State
initialState =
    State 0 0 0


manhattanDist : State -> Int
manhattanDist { x, y } =
    abs x + abs y


step : Action -> State -> State
step action state =
    case action of
        North n ->
            { state | y = state.y + n }

        South n ->
            { state | y = state.y - n }

        East n ->
            { state | x = state.x + n }

        West n ->
            { state | x = state.x - n }

        TurnLeft d ->
            { state | dir = state.dir - d |> modBy 360 }

        TurnRight d ->
            { state | dir = state.dir + d |> modBy 360 }

        Forward n ->
            case state.dir of
                270 ->
                    { state | y = state.y + n }

                90 ->
                    { state | y = state.y - n }

                0 ->
                    { state | x = state.x + n }

                180 ->
                    { state | x = state.x - n }

                _ ->
                    state


parseInput : String -> List Action
parseInput input =
    String.lines input
        |> List.map
            (\line ->
                case ( String.left 1 line, String.dropLeft 1 line |> String.toInt |> Maybe.withDefault -1 ) of
                    ( "N", n ) ->
                        North n

                    ( "S", n ) ->
                        South n

                    ( "E", n ) ->
                        East n

                    ( "W", n ) ->
                        West n

                    ( "L", n ) ->
                        TurnLeft n

                    ( "R", n ) ->
                        TurnRight n

                    ( "F", n ) ->
                        Forward n

                    _ ->
                        North 0
            )
