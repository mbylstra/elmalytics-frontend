module Elmalytics exposing (init, view, update, Model, Msg)

-- import Svg exposing (svg)
-- import Svg.Attributes exposing (width, height)
import Task exposing (Task)
import Http exposing (Error, get)
import Array exposing (Array)




import Html exposing (div, h1, text, p, node)

import Array
import Decoders exposing (totalsByMonthDecoder)

import Svg
import Svg.Attributes
import Plot exposing (..)

dataSources : { numReposCreatedPerMonth : String, numCommitsPerMonth : String }
dataSources =
  { numReposCreatedPerMonth =
    "http://elmalytics-backend.michaelbylstra.com/static/json/formattedNumReposCreatedPerMonth.json"
  , numCommitsPerMonth =
    "http://elmalytics-backend.michaelbylstra.com/static/json/formattedNumCommitsPerMonth.json"
  }

type alias TotalsByMonth =
  (Array (String, String, Int))

type alias Model =
  { numReposCreatedPerMonth : Maybe TotalsByMonth
  , numCommitsPerMonth : Maybe TotalsByMonth
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchSucceed dataSourceName rows ->
      let
        data =
          rows
          |> Array.fromList
          |> Array.slice 0 -1 -- remove last element as it's incomplete
      in
        case dataSourceName of
          "numReposCreatedPerMonth" ->
            { model | numReposCreatedPerMonth = Just data } ! []
          "numCommitsPerMonth" ->
            { model | numCommitsPerMonth = Just data } ! []
          _ ->
            Debug.crash ("no dataSourceName of " ++ dataSourceName)
    FetchFail error ->
      Debug.crash (toString error)

init : (Model, Cmd Msg)
init =
  let
    numReposCreatedPerMonthTask : Task Error (List (String, String, Int))
    numReposCreatedPerMonthTask =
        get totalsByMonthDecoder dataSources.numReposCreatedPerMonth
    numReposCreatedPerMonthCmd = Task.perform FetchFail (FetchSucceed "numReposCreatedPerMonth") numReposCreatedPerMonthTask

    numCommitsPerMonthTask : Task Error (List (String, String, Int))
    numCommitsPerMonthTask =
        get totalsByMonthDecoder dataSources.numCommitsPerMonth
    numCommitsPerMonthCmd = Task.perform FetchFail (FetchSucceed "numCommitsPerMonth") numCommitsPerMonthTask

  in
    { numCommitsPerMonth = Nothing
    , numReposCreatedPerMonth = Nothing
    }
    !
    [ numReposCreatedPerMonthCmd, numCommitsPerMonthCmd ]


type Msg =
    FetchSucceed String (List (String, String, Int))
  | FetchFail Http.Error


style : String
style = """
  body {
    font-family: sans-serif;
    margin: 20px;
    text-size: 10px;
  }
"""


type Label =
  NoMonthLabel | MonthLabel String | YearAndMonthLabel String String



plotView : TotalsByMonth -> Html.Html Msg
plotView totalsByMonth =
    let
      plotData : List (Float, Float)
      plotData =
        totalsByMonth
        |> Array.indexedMap (\i (year, month, total) -> (toFloat i, toFloat total))
        |> Array.toList

      customLabel : Float -> Svg.Svg Msg
      customLabel value =
        let
          yearAndMonth = Array.get (round value) totalsByMonth
            |> Maybe.map (\(year, month, _) -> (year, month))
            |> Maybe.withDefault ("", "")

          label : Label
          label =
            case yearAndMonth of
              (year, "Jan") -> YearAndMonthLabel year "Jan"
              (_, "Apr") -> MonthLabel "Apr"
              (_, "Jul") -> MonthLabel "Jul"
              (_, "Oct") -> MonthLabel "Oct"
              _ -> NoMonthLabel

          renderMonth month =
              Svg.text'
                  [ Svg.Attributes.transform ("translate(0, 27)")
                  , Svg.Attributes.style "text-anchor: middle; fill: #969696; font-size: 12px;"
                  ]
                  [ Svg.tspan [] [ Svg.text month ] ]

          renderYear year =
              Svg.text'
                  [ Svg.Attributes.transform ("translate(0, 60)")
                  , Svg.Attributes.style "text-anchor: middle; fill: #969696; font-size: 20px;"
                  ]
                  [ Svg.tspan [] [ Svg.text year ] ]
        in
          case label of
            NoMonthLabel ->
              Svg.text' [] []
            MonthLabel month ->
              renderMonth month
            YearAndMonthLabel year month ->
              Svg.g
                []
                [ renderMonth month
                , renderYear year
                ]

      customYAxisLabel value =
        Svg.text'
          [ Svg.Attributes.transform ("translate(-27, 0)")
          , Svg.Attributes.style "text-anchor: middle; fill: #969696; font-size: 12px;"
          ]
          [ Svg.tspan [] [ Svg.text <| toString value ] ]
    in
      plot
        [ size ( 1000, 600 )
        , padding (50, 100)
        , plotStyle [ ( "padding", "60px 60px 60px 60px" ), ( "overflow", "hidden" ) ]
        ]
        -- [ Plot.line [ areaStyle [ ( "stroke", "#cfd8ea" ), ( "fill", "#e4eeff" ) ] ] data
        -- [ horizontalGrid [ gridTickList [ 100, 200, 300, 400 ], gridStyle [ ( "stroke", "#e2e2e2" ) ] ]
        [ verticalGrid [ gridTickList [ 100, 200, 300, 400 ], gridStyle [ ( "stroke", "#e2e2e2" ) ] ]
        , line [ lineStyle [ ( "stroke", "#b6c9ef" ) ] ] plotData
        , xAxis
            [ axisLineStyle [ ( "stroke", "#7F7F7F" ) ]
            -- , tickList [ 0, 1, 2, 3, 4, 5, 6, 7 ]
            , amountOfTicks <| Array.length totalsByMonth
            , customViewLabel customLabel
            ]
        , yAxis
          [ axisLineStyle [ ( "stroke", "#b9b9b9" ) ]
          , customViewLabel customYAxisLabel
          ]
        ]

view : Model -> Html.Html Msg
view model =

  let
    numReposCreatedPerMonthHtml =
      case model.numReposCreatedPerMonth of
        Just data ->
          plotView data
        Nothing ->
          div [] []
    numCommitsPerMonthHtml =
      case model.numCommitsPerMonth of
        Just data ->
          plotView data
        Nothing ->
          div [] []
  in
    div []
      [ node "style" [] [ text style ]
      , h1 [ ] [ text "Elm-alytics" ]
      , numReposCreatedPerMonthHtml
      , numCommitsPerMonthHtml
      ]
