module Day8 exposing (solution)

import Array exposing (Array)
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        prg =
            parseInput input

        initialState =
            { pc = 0, acc = 0, visited = Set.empty }

        infinitLoopIdentified : State -> Bool
        infinitLoopIdentified state =
            Set.member state.pc state.visited
    in
    initialState
        |> stepUntil prg infinitLoopIdentified
        |> .acc
        |> String.fromInt


part2 : Solver
part2 input =
    "not implemented"


type alias Program =
    Array Op


type alias State =
    { pc : Int
    , acc : Int
    , visited : Set Int
    }


type Op
    = Nop
    | Acc Int
    | Jmp Int


stepUntil : Program -> (State -> Bool) -> State -> State
stepUntil prg haltCondtition state =
    if haltCondtition state then
        state

    else
        stepUntil prg haltCondtition (step prg state)


step : Program -> State -> State
step prg ({ pc, acc, visited } as state) =
    case Array.get pc prg of
        Just (Acc n) ->
            { state | pc = pc + 1, acc = acc + n, visited = Set.insert pc visited }

        Just (Jmp n) ->
            { state | pc = pc + n, visited = Set.insert pc visited }

        _ ->
            { state | pc = pc + 1, visited = Set.insert pc visited }


parseInput : String -> Program
parseInput input =
    input
        |> String.lines
        |> List.map parseOp
        |> Array.fromList


parseOp : String -> Op
parseOp opStr =
    case String.split " " opStr of
        "acc" :: param :: [] ->
            Acc (String.toInt param |> Maybe.withDefault 0)

        "jmp" :: param :: [] ->
            Jmp (String.toInt param |> Maybe.withDefault 0)

        _ ->
            Nop
