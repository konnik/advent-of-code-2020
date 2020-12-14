module Day14 exposing (solution)

import Dict exposing (Dict)
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
                    let
                        maskedValue =
                            value |> combineWithBitmask state.bitmask
                    in
                    state
                        |> writeMem address maskedValue
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
                    address
                        |> toFloatingAddress state.bitmask
                        |> resolveFloatingAdresses
                        |> List.foldl (\resolvedAddress newState -> writeMem resolvedAddress value newState) state
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
    Dict (List Int) Binary


type Bit
    = Zero
    | One


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


writeMem : Binary -> Binary -> State -> State
writeMem adress value state =
    { state | mem = Dict.insert (adress |> toInts) value state.mem }


resolveFloatingAdresses : Bitmask -> List Binary
resolveFloatingAdresses bitmask =
    case bitmask of
        [] ->
            [ [] ]

        (Just bit) :: rest ->
            resolveFloatingAdresses rest |> List.map ((::) bit)

        Nothing :: rest ->
            List.map ((::) Zero) (resolveFloatingAdresses rest)
                ++ List.map ((::) One) (resolveFloatingAdresses rest)


toFloatingAddress : Bitmask -> Binary -> Bitmask
toFloatingAddress bitmask adress =
    List.map2
        (\mask adressbit ->
            mask
                |> Maybe.map
                    (\maskbit ->
                        if maskbit == Zero then
                            adressbit

                        else
                            One
                    )
        )
        bitmask
        adress


combineWithBitmask : Bitmask -> Binary -> Binary
combineWithBitmask bitmask value =
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


fromBits : Binary -> Int
fromBits bits =
    bits
        |> List.foldl
            (\bit acc ->
                bitAs 0 1 bit + acc * 2
            )
            0


toInts : Binary -> List Int
toInts binary =
    binary
        |> List.map (bitAs 0 1)


bitAs : a -> a -> Bit -> a
bitAs zero one =
    \bit ->
        case bit of
            Zero ->
                zero

            One ->
                one


bitFrom : Int -> Int -> Bit
bitFrom zeroValue n =
    if n == zeroValue then
        Zero

    else
        One


toBits : Int -> Binary
toBits value =
    let
        f : Int -> ( Int, Binary ) -> ( Int, Binary )
        f _ ( remainder, bits ) =
            ( remainder // 2, bitFrom 0 (remainder |> modBy 2) :: bits )
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
    String.dropLeft 7 str
        |> String.toList
        |> List.map
            (\ch ->
                case ch of
                    '0' ->
                        Just Zero

                    '1' ->
                        Just One

                    _ ->
                        Nothing
            )
        |> Mask


parseMem : String -> Instr
parseMem str =
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
