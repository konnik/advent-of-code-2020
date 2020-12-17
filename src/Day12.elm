module Day12 exposing (solution)

import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInput input
        |> List.foldl step1 initialState1
        |> manhattanDist
        |> String.fromInt


part2 : Solver
part2 input =
    parseInput input
        |> List.foldl step2 initialState2
        |> manhattanDist2
        |> String.fromInt


type alias State =
    { x : Int, y : Int, dir : Int }


type alias State2 =
    { x : Int, y : Int, dx : Int, dy : Int }


type Action
    = North Int
    | South Int
    | East Int
    | West Int
    | TurnLeft Int
    | TurnRight Int
    | Forward Int


initialState1 : State
initialState1 =
    State 0 0 0


initialState2 : State2
initialState2 =
    State2 0 0 10 1


manhattanDist : State -> Int
manhattanDist { x, y } =
    abs x + abs y


manhattanDist2 : State2 -> Int
manhattanDist2 { x, y } =
    abs x + abs y


step1 : Action -> State -> State
step1 action state =
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


step2 : Action -> State2 -> State2
step2 action state =
    case action of
        North n ->
            { state | dy = state.dy + n }

        South n ->
            { state | dy = state.dy - n }

        East n ->
            { state | dx = state.dx + n }

        West n ->
            { state | dx = state.dx - n }

        {- Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees. -}
        TurnLeft d ->
            case d of
                90 ->
                    { state | dy = state.dx, dx = -state.dy }

                180 ->
                    { state | dy = -state.dy, dx = -state.dx }

                _ ->
                    { state | dy = -state.dx, dx = state.dy }

        {- Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees. -}
        TurnRight d ->
            case d of
                90 ->
                    { state | dy = -state.dx, dx = state.dy }

                180 ->
                    { state | dy = -state.dy, dx = -state.dx }

                _ ->
                    { state | dy = state.dx, dx = -state.dy }

        {- Action F means to move forward to the waypoint a number of times equal to the given value. -}
        Forward n ->
            { state | x = state.x + state.dx * n, y = state.y + state.dy * n }


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
