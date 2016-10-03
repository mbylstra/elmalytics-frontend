module Top exposing (init, view, update)

import Svg exposing (svg)
import Svg.Attributes exposing (width, height)

import Chart


import Html exposing (div, h1, text, p, node)

import Array
import Data exposing (repoCreationsByMonth)


type alias Model = Bool

update : Msg -> Model -> Model
update msg model =
  model

init : Model
init =
  False


type Msg = NoOp


intToMonth i =

  case (Debug.log "month" i) of
    1 -> "Jan"
    2 -> "Feb"
    3 -> "Mar"
    4 -> "Apr"
    5 -> "May"
    6 -> "Jun"
    7 -> "Jul"
    8 -> "Aug"
    9 -> "Sep"
    10  -> "Oct"
    11 -> "Nov"
    12 -> "Dec"
    _ -> ""

chartData : List (Float, String)
chartData =
  List.map
    (\row ->
      let
        rowArray = Array.fromList row
        value =
          rowArray
            |> Array.get 2
            |> Maybe.withDefault 0
            |> toFloat
        year =
          rowArray
            |> Array.get 0
            |> Maybe.withDefault 0
            |> toString
        month =
          rowArray
            |> Array.get 1
            |> Maybe.withDefault 0
            |> intToMonth
      in
        (value, month ++ " " ++ year ++ ":")
    )
    repoCreationsByMonth


style = """
  body {
    font-family: sans-serif;
    margin: 20px;
    text-size: 10px;
  }
"""


chartView data =
  -- Chart.vBar data
  Chart.hBar data
    -- |> Chart.addValueToLabel
    |> Chart.title "Total Elm Github repos created by month"
    |> Chart.toHtml
  -- Chart.lChart data |> Chart.toHtml

view model =
  div []
    [ node "style" [] [ text style ]
    , h1 [ ] [ text "Elm-alytics" ]
    , chartView chartData
    ]
