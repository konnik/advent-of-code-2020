module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Element, column, el, padding, row, spacing, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Input
import Types exposing (Solution)
import Url


solutions : Dict Int Solution
solutions =
    Dict.fromList
        []


type alias Model =
    { input : String
    , day : Maybe Int
    , answer1 : String
    , answer2 : String
    , key : Nav.Key
    }


type Msg
    = NoOp
    | InputLoaded Int (Result Http.Error String)
    | UrlChanged Url.Url
    | DaySelected Int


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { key = key
            , input = ""
            , day = Nothing
            , answer1 = ""
            , answer2 = ""
            }
    in
    initFromUrl model url


initFromUrl : Model -> Url.Url -> ( Model, Cmd Msg )
initFromUrl model url =
    let
        selectedDay =
            url.fragment
                |> Maybe.andThen String.toInt
    in
    ( { model
        | input = ""
        , day = selectedDay
        , answer1 = ""
        , answer2 = ""
      }
    , selectedDay |> Maybe.map (\day -> Input.load day InputLoaded) |> Maybe.withDefault Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DaySelected day ->
            ( model, Nav.pushUrl model.key ("#" ++ String.fromInt day) )

        UrlChanged url ->
            initFromUrl model url

        InputLoaded day (Ok input) ->
            let
                ( a1, a2 ) =
                    solveDay day input
            in
            ( { model
                | input = input
                , answer1 = a1
                , answer2 = a2
              }
            , Cmd.none
            )

        InputLoaded day (Err _) ->
            ( { model | input = "Could not load input for day " ++ String.fromInt day }, Cmd.none )


solveDay : Int -> String -> ( String, String )
solveDay day input =
    Dict.get day solutions
        |> Maybe.andThen
            (\( part1, part2 ) ->
                Just ( part1 input, part2 input )
            )
        |> Maybe.withDefault ( "", "" )


view : Model -> Browser.Document Msg
view model =
    { title = "AoE 2019"
    , body = [ Element.layout [ padding 20 ] (mainView model) ]
    }


mainView : Model -> Element Msg
mainView model =
    column [ spacing 20, Font.family [ Font.monospace ] ]
        [ header
        , navigation
        , case model.day of
            Nothing ->
                text "Ingen dag vald..."

            Just day ->
                column [ spacing 20, Font.family [ Font.monospace ] ]
                    [ text <| "Solution for day " ++ String.fromInt day
                    , text <| "Part 1: "
                    , text <| model.answer1
                    , text <| "Part 2: "
                    , text <| model.answer2
                    , text "Input: "
                    , text model.input
                    ]
        ]


header : Element Msg
header =
    el [ Font.size 30, Font.bold ] (text "Advent of Code in Elm - 2020")


navButton : Int -> Element Msg
navButton day =
    Input.button [ Border.width 1, Border.rounded 4 ]
        { label = el [ padding 5 ] <| text (String.fromInt day)
        , onPress = Just (DaySelected day)
        }


navigation : Element Msg
navigation =
    row [ spacing 5 ]
        (List.map navButton (Dict.keys solutions))


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = UrlChanged
        }
