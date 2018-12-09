module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Days.Day1 as Day1 exposing (day)
import Days.Day2 as Day2 exposing (day)
import Html exposing (..)
import Html.Attributes exposing (selected, src, value)
import Html.Events exposing (onClick, onInput)
import Util.Day exposing (Day)
import Util.SelectList as SelectList exposing (SelectList)



---- MODEL ----


type alias Model =
    { days : SelectList Day
    , input : String
    , answer1 : String
    , answer2 : String
    }


init : ( Model, Cmd Msg )
init =
    ( { days = SelectList.fromLists [] Day1.day [ Day2.day ]
      , input = ""
      , answer1 = ""
      , answer2 = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = DayChanged String
    | InputChanged String
    | RunClicked
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged input ->
            ( { model | input = input }, Cmd.none )

        DayChanged dayNumberStr ->
            let
                dayAsInt =
                    dayNumberStr |> String.toInt

                newDays =
                    case dayAsInt of
                        Nothing ->
                            model.days

                        Just dayNumber ->
                            SelectList.select (\day -> day.number == dayNumber) model.days
            in
            ( { model | days = newDays }, Cmd.none )

        RunClicked ->
            let
                day =
                    SelectList.selected model.days

                answer1 =
                    day.solvePart1 model.input

                answer2 =
                    day.solvePart2 model.input
            in
            ( { model | answer1 = answer1, answer2 = answer2 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Select day" ]
        , select [ onInput DayChanged ]
            (List.map
                (viewDayOption False)
                (SelectList.before model.days)
                ++ [ viewDayOption True (SelectList.selected model.days) ]
                ++ List.map
                    (viewDayOption False)
                    (SelectList.after model.days)
            )
        , div []
            [ textarea [ onInput InputChanged ] []
            , button [ onClick RunClicked ] [ text "Run" ]
            , viewAnswers model.answer1 model.answer2
            ]
        ]


viewAnswers : String -> String -> Html Msg
viewAnswers answer1 answer2 =
    div []
        [ div [] [ "Part 1: " ++ answer1 |> text ]
        , div [] [ "Part 2: " ++ answer2 |> text ]
        ]


viewDayOption : Bool -> Day -> Html Msg
viewDayOption isSelected day =
    option
        [ selected isSelected
        , value (String.fromInt day.number)
        ]
        [ "Day " ++ String.fromInt day.number |> text ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
