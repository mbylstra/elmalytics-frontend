module Top exposing (init, view, update)

import Svg exposing (svg)
import Svg.Attributes exposing (width, height)
import Chart exposing (Scale, Data)
import BarChart exposing (barChart, color, width)
import LineChart exposing (lineChart, color, width)
import ScatterPlot exposing (scatterPlot, color, size)


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

-- data : Data msg
-- data =
--   [
--     (1, 22.2, []),
--     (2, 34, []),
--     (3, 56, [BarChart.color "yellowgreen"]),
--     (4, 62, []),
--     (5, 77, [])
--   ]

data : Data msg
data =
  List.indexedMap
    (\index row ->
      ( toFloat index
      , row
        |> Array.fromList
        |> Array.get 2
        |> Maybe.withDefault 0
        |> toFloat
      , []
      )
    )
    repoCreationsByMonth


xScale : Scale
xScale x =
  20 + x * 20


yScale : Scale
yScale y =
  600 - y * 3



style = """
  body {
    font-family: sans-serif;
    margin: 20px;
  }
"""

view model =
  div []
    [ node "style" [] [ text style ]
    , h1 [ ] [ text "Elm-alytics" ]
    , p [ ] [ text "Elm git repos created by month" ]
    , svg
      [
        Svg.Attributes.width "1000",
        Svg.Attributes.height "600"
      ]
      [
        -- barChart
        --   [
        --     BarChart.color "pink",
        --     BarChart.width "5"
        --   ]
        --   { data = data
        --   , xScale = xScale
        --   , yScale = yScale
        --   },
        lineChart
          [
            LineChart.color "#7E94C7"
          ]
          { data = data
          , xScale = (\x -> 20 + x * 15)
          , yScale = (\y -> 500 - y * 1)
          }
        -- scatterPlot
        --   []
        --   { data = data
        --   , xScale = xScale
        --   , yScale = (\y -> 400 - y * 3)
        --   }
      ]
  ]
