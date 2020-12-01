module Types exposing (..)


type alias Solution =
    ( Solver, Solver )


type alias Solver =
    String -> String
