module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Day1
import Day2
import Day3
import Day4
import Day5
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, column, el, fill, height, maximum, padding, rgb255, row, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button, labelAbove, labelHidden, placeholder)
import Html.Attributes exposing (value)
import Http
import Input
import List
import Types exposing (Solution)
import Url


solutions : Dict Int Solution
solutions =
    Dict.fromList
        [ ( 1, Day1.solution )
        , ( 2, Day2.solution )
        , ( 3, Day3.solution )
        , ( 4, Day4.solution )
        , ( 5, Day5.solution )
        ]


type alias Model =
    { input : String
    , day : Maybe Int
    , answer1 : String
    , answer2 : String
    , key : Nav.Key
    , interactiveInput : Bool
    }


type Msg
    = NoOp
    | InputLoaded Int (Result Http.Error String)
    | InputChanged Int String
    | UrlChanged Url.Url
    | DaySelected Int
    | SolveDay Int


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { key = key
            , input = ""
            , day = Nothing
            , answer1 = ""
            , answer2 = ""
            , interactiveInput = False
            }
    in
    initFromUrl model url


initFromUrl : Model -> Url.Url -> ( Model, Cmd Msg )
initFromUrl model url =
    let
        lastDayWithSolution =
            Dict.keys solutions
                |> List.sort
                |> List.reverse
                |> List.head

        selectedDayInUrl =
            url.fragment
                |> Maybe.andThen String.toInt

        initialDay : Maybe Int
        initialDay =
            [ selectedDayInUrl, lastDayWithSolution ]
                |> List.filterMap identity
                |> List.head
    in
    ( { model
        | input = ""
        , day = initialDay
        , answer1 = ""
        , answer2 = ""
      }
    , initialDay |> Maybe.map (\day -> Input.load day InputLoaded) |> Maybe.withDefault Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SolveDay day ->
            let
                ( a1, a2 ) =
                    solveDay day model.input
            in
            ( { model
                | answer1 = a1
                , answer2 = a2
              }
            , Cmd.none
            )

        InputChanged day newInput ->
            ( { model | input = newInput }, Cmd.none )

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
            ( { model
                | input = ""
                , interactiveInput = True
              }
            , Cmd.none
            )


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
    let
        dayStr =
            model.day
                |> Maybe.map (\d -> "Day " ++ String.fromInt d ++ " - ")
                |> Maybe.withDefault ""
    in
    { title = dayStr ++ "Advent of Code 2020 in Elm"
    , body =
        [ Element.layout
            [ padding 20
            , width fill
            ]
            (mainView model)
        ]
    }


mainView : Model -> Element Msg
mainView model =
    column [ width fill, spacing 20, Font.family [ Font.monospace ] ]
        [ header
        , navigation
        , case model.day of
            Nothing ->
                text "Please select a day above..."

            Just day ->
                column
                    [ width fill
                    , spacing 20
                    , Font.family [ Font.monospace ]
                    ]
                    [ text <| "Solution for day " ++ String.fromInt day
                    , text <| "Part 1: "
                    , text <| model.answer1
                    , text <| "Part 2: "
                    , text <| model.answer2
                    , text "Input: "
                    , if model.interactiveInput then
                        interactiveInputView day model.input

                      else
                        text model.input
                    ]
        ]


interactiveInputView : Int -> String -> Element Msg
interactiveInputView day value =
    let
        labelText =
            "Enter your input for day " ++ String.fromInt day ++ " here..."
    in
    row [ width fill, spacing 30 ]
        [ Input.multiline
            [ width (fill |> maximum 700)
            , height (fill |> maximum 400)
            ]
            { onChange = InputChanged day
            , label = labelHidden <| labelText
            , placeholder = Just (placeholder [] (text <| labelText))
            , text = value
            , spellcheck = False
            }
        , solveButton day
        ]


solveButton : Int -> Element Msg
solveButton day =
    button
        [ Border.color (rgb255 0 0 0)
        , Border.width 1
        , Border.rounded 5
        , padding 15
        , alignTop
        ]
        { onPress = Just (SolveDay day)
        , label = text ("Solve day " ++ String.fromInt day)
        }


header : Element Msg
header =
    column []
        [ el [ Font.size 30, Font.bold ] (text "Advent of Code in Elm - 2020")
        , Element.newTabLink [ alignRight, Font.size 10 ]
            { url = "https://github.com/konnik/advent-of-code-2020"
            , label = text "Source on GitHub"
            }
        ]


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
