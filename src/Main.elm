module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Days.Day2 exposing (day)
import Html exposing (..)
import Html.Attributes exposing (selected, src, value)
import Html.Events exposing (onClick, onInput)
import Util.Day exposing (Day)
import Util.SelectList as SelectList exposing (SelectList)



---- MODEL ----


day1 : Day
day1 =
    { number = 1
    , solvePart1 = \_ -> "Solved day 1 part 1"
    , solvePart2 = \_ -> "Solved day 1 part 2"
    }


day2 : Day
day2 =
    { number = 2
    , solvePart1 = \_ -> "Solved day 2 part 1"
    , solvePart2 = \_ -> "Solved day 2 part 2"
    }


type alias Model =
    { days : SelectList Day
    , input : String
    , answer : String
    }


init : ( Model, Cmd Msg )
init =
    ( { days = SelectList.fromLists [] day1 [ day2 ]
      , input = ""
      , answer = ""
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

                answer =
                    day.solvePart1 model.input
            in
            ( { model | answer = answer }, Cmd.none )

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
            , viewAnswer model.answer
            ]
        ]


viewAnswer : String -> Html Msg
viewAnswer answer =
    div [] [ text answer ]


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
