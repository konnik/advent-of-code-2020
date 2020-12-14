module Day14 exposing (solution)

import Dict exposing (Dict)
import Html exposing (address)
import String exposing (startsWith)
import Types exposing (Solution, Solver)


solution : Solution
solution =
    ( part1, part2 )


part1 : Solver
part1 input =
    let
        execute : Instr -> State -> State
        execute instr state =
            case instr of
                Mask newBitmask ->
                    { state | bitmask = newBitmask }

                Mem address value ->
                    { state | mem = Dict.insert address (combineValue state.bitmask value) state.mem }
    in
    input
        |> parseInput
        |> List.foldl execute initalState
        |> memorySumAsString


part2 : Solver
part2 input =
    let
        execute : Instr -> State -> State
        execute instr state =
            case instr of
                Mask newBitmask ->
                    { state | bitmask = newBitmask }

                Mem address value ->
                    let
                        newMem =
                            combineAddress state.bitmask address
                                |> resolveFloatingAdresses
                                |> List.foldl (\resolvedAddress mem -> Dict.insert resolvedAddress value mem) state.mem
                    in
                    { state | mem = newMem }
    in
    input
        |> parseInput
        |> List.foldl execute initalState
        |> memorySumAsString


type alias State =
    { mem : Memory
    , bitmask : Bitmask
    }


type alias Memory =
    Dict Binary Binary


type alias Bit =
    Int


type alias Binary =
    List Bit


type alias Bitmask =
    List (Maybe Bit)


type Instr
    = Mask Bitmask
    | Mem Binary Binary


memorySumAsString : State -> String
memorySumAsString state =
    state.mem
        |> Dict.values
        |> List.map fromBits
        |> List.sum
        |> String.fromInt


resolveFloatingAdresses : Bitmask -> List Binary
resolveFloatingAdresses bitmask =
    case bitmask of
        [] ->
            [ [] ]

        (Just 0) :: rest ->
            resolveFloatingAdresses rest |> List.map ((::) 0)

        (Just 1) :: rest ->
            List.map ((::) 1) (resolveFloatingAdresses rest)

        Nothing :: rest ->
            List.map ((::) 0) (resolveFloatingAdresses rest)
                ++ List.map ((::) 1) (resolveFloatingAdresses rest)

        (Just _) :: _ ->
            [ [ -1 ] ]


combineAddress : Bitmask -> Binary -> Bitmask
combineAddress bitmask adress =
    List.map2
        (\mask adressbit ->
            mask
                |> Maybe.map
                    (\maskbit ->
                        if maskbit == 0 then
                            adressbit

                        else
                            1
                    )
        )
        bitmask
        adress


combineValue : Bitmask -> Binary -> Binary
combineValue bitmask value =
    List.map2
        (\mask bit ->
            mask |> Maybe.map identity |> Maybe.withDefault bit
        )
        bitmask
        value


initalState : State
initalState =
    { mem = Dict.empty
    , bitmask = emptyBitmask
    }


emptyBitmask : Bitmask
emptyBitmask =
    List.repeat 36 Nothing


fromBits : List Int -> Int
fromBits bits =
    bits
        |> List.foldl
            (\bit acc ->
                bit + acc * 2
            )
            0


toBits : Int -> Binary
toBits value =
    let
        f : Int -> ( Int, List Int ) -> ( Int, List Int )
        f _ ( remainder, bits ) =
            ( remainder // 2, (remainder |> modBy 2) :: bits )
    in
    List.range 1 36
        |> List.foldl f ( value, [] )
        |> Tuple.second



-- parse input


parseInput : String -> List Instr
parseInput input =
    input
        |> String.lines
        |> List.map parseLine


parseLine : String -> Instr
parseLine line =
    line
        |> (if String.startsWith "mask" line then
                parseBitmask

            else
                parseMem
           )


parseBitmask : String -> Instr
parseBitmask str =
    --mask = 00110X11X0000110X0000001000111010X00
    String.dropLeft 7 str
        |> String.toList
        |> List.map
            (\ch ->
                case ch of
                    '0' ->
                        Just 0

                    '1' ->
                        Just 1

                    _ ->
                        Nothing
            )
        |> Mask


parseMem : String -> Instr
parseMem str =
    --mem[47790] = 1221939
    str
        |> String.dropLeft 4
        |> String.split "] = "
        |> List.map (String.toInt >> Maybe.withDefault 0)
        |> (\ints ->
                case ints of
                    [ address, value ] ->
                        Mem (toBits address) (toBits value)

                    _ ->
                        Mem [] []
           )
