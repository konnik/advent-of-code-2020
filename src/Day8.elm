module Day8 exposing (solution)

import Array exposing (Array)
import Set exposing (Set)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    parseInput input
        |> runProgram
        |> valueOfAccumulator
        |> String.fromInt


part2 : Solver
part2 input =
    let
        originalProgram =
            parseInput input

        mutatedPrograms : List Program
        mutatedPrograms =
            instructionAddesses originalProgram
                |> List.filterMap (mutateInstructionAt originalProgram)
    in
    mutatedPrograms
        |> List.map runProgram
        |> List.filter programHalted
        |> List.head
        |> Maybe.map (valueOfAccumulator >> String.fromInt)
        |> Maybe.withDefault "no answer found"


type alias Program =
    Array Op


type alias State =
    { pc : Int
    , acc : Int
    , visited : Set Int
    , halted : Bool
    }


type Op
    = Nop Int
    | Acc Int
    | Jmp Int


initialState : State
initialState =
    { pc = 0, acc = 0, visited = Set.empty, halted = False }


runProgram : Program -> State
runProgram prg =
    initialState
        |> stepUntil prg (infinitLoopIdentified |> or programHalted)


infinitLoopIdentified : State -> Bool
infinitLoopIdentified state =
    Set.member state.pc state.visited


programHalted : State -> Bool
programHalted state =
    state.halted


valueOfAccumulator : State -> Int
valueOfAccumulator =
    .acc


instructionAddesses : Program -> List Int
instructionAddesses prg =
    List.range 0 (Array.length prg - 1)


or : (State -> Bool) -> (State -> Bool) -> State -> Bool
or a b state =
    a state || b state


mutateInstructionAt : Program -> Int -> Maybe Program
mutateInstructionAt prg pos =
    case Array.get pos prg of
        Just (Nop n) ->
            Just <| Array.set pos (Jmp n) prg

        Just (Jmp n) ->
            Just <| Array.set pos (Nop n) prg

        _ ->
            Nothing


stepUntil : Program -> (State -> Bool) -> State -> State
stepUntil prg haltCondtition state =
    if haltCondtition state then
        state

    else
        stepUntil prg haltCondtition (step prg state)


step : Program -> State -> State
step prg ({ pc, acc, visited, halted } as state) =
    if halted then
        state

    else if pc >= Array.length prg then
        { state | halted = True }

    else
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

        "nop" :: param :: [] ->
            Nop (String.toInt param |> Maybe.withDefault 0)

        _ ->
            Nop 0
