module Elmalytics exposing (init, view, update, Model, Msg)

-- import Svg exposing (svg)
-- import Svg.Attributes exposing (width, height)
import Task exposing (Task)
import Http exposing (Error, get)
import Array exposing (Array)




import Html exposing (div, h1, text, p, node, img, h2, h3, a)
import Html.Attributes exposing (src, class, class, width, height, href)

import Array
import Decoders exposing (..)

import Svg
import Svg.Attributes
import Plot exposing (..)
import Github exposing (avatarUrl, userUrl, repoUrl, githubStarButton)

dataSources :
  { numReposCreatedPerMonth : String
  , numCommitsPerMonth : String
  , mostStarredRepos : String
  , mostReposCreated : String
  , mostStarsForRepos : String
  }
dataSources =
  { numReposCreatedPerMonth =
    "http://elmalytics-backend.michaelbylstra.com/static/json/formattedNumReposCreatedPerMonth.json"
  , numCommitsPerMonth =
    "http://elmalytics-backend.michaelbylstra.com/static/json/formattedNumCommitsPerMonth.json"
  , mostStarredRepos =
    "http://elmalytics-backend.michaelbylstra.com/static/json/mostStarredRepos.json"
  , mostReposCreated =
    "http://elmalytics-backend.michaelbylstra.com/static/json/mostReposCreated.json"
  , mostStarsForRepos =
    "http://elmalytics-backend.michaelbylstra.com/static/json/mostStarsForRepos.json"
  }

type alias TotalsByMonth =
  (Array (String, String, Int))

type alias Model =
  { numReposCreatedPerMonth : Maybe TotalsByMonth
  , numCommitsPerMonth : Maybe TotalsByMonth
  , mostStarredRepos : Maybe (List (String, String, Int))
  , mostReposCreated : Maybe (List (String, Int))
  , mostStarsForRepos : Maybe (List (String, Int))
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
    MostStarredReposSucceed _ rows ->
      { model | mostStarredRepos = Just rows } ! []
    MostReposCreatedSucceed _ rows ->
      { model | mostReposCreated = Just rows } ! []
    MostStarsForReposSucceed _ rows ->
      { model | mostStarsForRepos = Just rows } ! []
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

    mostStarredReposTask : Task Error (List (String, String, Int))
    mostStarredReposTask =
        get mostStarredReposDecoder dataSources.mostStarredRepos
    mostStarredReposCmd = Task.perform FetchFail (MostStarredReposSucceed "mostStarredRepos") mostStarredReposTask

    mostReposCreatedTask : Task Error (List (String, Int))
    mostReposCreatedTask =
        get mostReposCreatedDecoder dataSources.mostReposCreated
    mostReposCreatedCmd = Task.perform FetchFail (MostReposCreatedSucceed "mostReposCreated") mostReposCreatedTask

    mostStarsForReposTask : Task Error (List (String, Int))
    mostStarsForReposTask =
        get mostStarsForReposDecoder dataSources.mostStarsForRepos
    mostStarsForReposCmd = Task.perform FetchFail (MostStarsForReposSucceed "mostStarsForRepos") mostStarsForReposTask

  in
    { numCommitsPerMonth = Nothing
    , numReposCreatedPerMonth = Nothing
    , mostStarredRepos = Nothing
    , mostReposCreated = Nothing
    , mostStarsForRepos = Nothing
    }
    !
    [ numReposCreatedPerMonthCmd
    , numCommitsPerMonthCmd
    , mostStarredReposCmd
    , mostReposCreatedCmd
    , mostStarsForReposCmd
    ]


type Msg =
    FetchSucceed String (List (String, String, Int))
  | MostStarredReposSucceed String (List (String, String, Int))
  | MostReposCreatedSucceed String (List (String, Int))
  | MostStarsForReposSucceed String (List (String, Int))
  | FetchFail Http.Error


style : String
style = """


  @import url('https://fonts.googleapis.com/css?family=Cormorant+Garamond:400,700');
  body {
    font-family: sans-serif;
    margin: 20px;
    text-size: 10px;
    color: #555;
    padding: 0px;
  }
  body > div {
    padding-bottom: 120px;
  }

  .header {
    margin-top: 32px;
    display: flex;
    flex-direction: row;
    /* align-items: center; */
    align-items: baseline;
    justify-content: center;
  }


  .logo {
    width: 50px;
    padding-right: 16px;
  }

  .github-star-button {
    position: absolute;
    right: 0px;
    top: 30px;
    z-index: 1;
  }


  a {
    color: #4078c0;
    text-decoration: none;
  }

  h1, h2 {
    font-family: "Cormorant Garamond", serif;
    font-weight: 700;
  }

  h1 {
    font-size: 65px;
    margin-bottom: -10px;
  }

  h2 {
    text-align: center;
    font-size: 35px;
  }

  .plots {
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    margin-bottom: 100px;
  }

  .plot-container {
    padding: 50px 0px;
  }

  .avatar-list {
    margin-top: 70px;
    display: flex;
    flex-direction: row;
    flex-wrap: wrap;
    justify-content: center;
  }

  .repo {
    display: flex;
    flex-direction: row;
    align-items: center;
    margin-bottom: 30px;
    margin-right: 20px;
    width: 200px;
  }

  .repo > div > div {
    height: 23px;
  }

  .avatar {
    width: 50px;
    height: 50px;
    margin-right: 13px;
    border-radius: 25px;
  }

  .avatar-list-wide .repo {
    width: 300px;
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
        [ size ( 800, 400 )
        , padding (0, 20)
        , plotStyle [ ( "padding", "0px 40px 60px 60px" ), ( "overflow", "hidden" ) ]
        ]
        -- [ Plot.line [ areaStyle [ ( "stroke", "#cfd8ea" ), ( "fill", "#e4eeff" ) ] ] data
        -- [ horizontalGrid [ gridTickList [ 100, 200, 300, 400 ], gridStyle [ ( "stroke", "#e2e2e2" ) ] ]
        -- [ verticalGrid [ gridTickList [ 100, 200, 300, 400 ], gridStyle [ ( "stroke", "#e2e2e2" ) ] ]
        [ line [ lineStyle [ ( "stroke", "#b6c9ef" ) ] ] plotData
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

    mostStarredReposHtml =
      case model.mostStarredRepos of
        Just data ->
          mostStarredReposView data
        Nothing ->
          div [] []

    mostReposCreatedHtml =
      case model.mostReposCreated of
        Just data ->
          mostReposCreatedView data
        Nothing ->
          div [] []

    mostStarsForReposHtml =
      case model.mostStarsForRepos of
        Just data ->
          mostStarsForReposView data
        Nothing ->
          div [] []

    logo =
      img [ class "logo", src "elm-logo.svg" ] [ ]
  in
    div []
      [ node "style" [] [ text style ]
      , div [ class "github-star-button" ]
        [ githubStarButton
          { user="mbylstra"
          , repo="elmalytics-frontend"
          , type'="star"
          , size="small"
          , style=[("vertical-align", "middle"), ("margin-top", "-5px")]
          }
        ]

      , div [ class "header" ]
        [ logo
        , h1 [ ] [ text "Elmalytics" ]
        ]
      , div [ class "plots" ]
        [ div [ class "plot-container" ]
          [ h2 [] [ text "Number of Elm repos created on Github by month" ]
          , numReposCreatedPerMonthHtml
          ]
        , div [ class "plot-container" ]
          [ h2 [] [ text "Total number of Git commits to Elm projects by month" ]
          , numCommitsPerMonthHtml
          ]
        ]
        , div [ class "plot-container" ]
          [ h2 [] [ text "Most starred Elm repos" ]
          , mostStarredReposHtml
          ]
        , div [ class "plot-container" ]
          [ h2 [] [ text "Most stars gathered for Elm repos" ]
          , mostStarsForReposHtml
          ]
        , div [ class "plot-container" ]
          [ h2 [] [ text "Most Elm repos created" ]
          , mostReposCreatedHtml
          ]
      ]

mostStarredReposView : List (String, String, Int) -> Html.Html Msg
mostStarredReposView mostStarredRepos =
  let
    avatarWidth = 50
    renderRow : (String, String, Int) -> Html.Html Msg
    renderRow (repoName, userName, total) =
      div [ class "repo" ]
        [ img
          [ class "avatar"
          , src <| avatarUrl userName avatarWidth
          ]
          []
        , div [ ]
          [ div [ ]
            [ img [ src "github-star.svg" ] []
            , text " "
            , text <| toString total
            ]
          , div [ ]
            [ a [ href <| repoUrl userName repoName ] [ text repoName ]
            , text " by "
            , a [ href <| userUrl userName ] [ text userName ]
            ]
          ]
        ]
  in
    div [ class "avatar-list avatar-list-wide" ]
      (List.map renderRow mostStarredRepos)

mostReposCreatedView : List (String, Int) -> Html.Html Msg
mostReposCreatedView mostReposCreated =
  let
    avatarWidth = 50
    renderRow : (String, Int) -> Html.Html Msg
    renderRow (userName, total) =
      div [ class "repo" ]
        [ img
          [ class "avatar"
          , src <| avatarUrl userName avatarWidth
          ]
          []
        , div [ ]
          [ div [ ]
            [ text <| toString total
            , text " repos"
            ]
          , div [ ]
            [ a [ href <| userUrl userName ] [ text userName ]
            ]
          ]
        ]
  in
    div [ class "avatar-list" ]
      (List.map renderRow mostReposCreated)

mostStarsForReposView : List (String, Int) -> Html.Html Msg
mostStarsForReposView mostStarsForRepos =
  let
    avatarWidth = 50
    renderRow : (String, Int) -> Html.Html Msg
    renderRow (userName, total) =
      div [ class "repo" ]
        [ img
          [ class "avatar"
          , src <| avatarUrl userName avatarWidth
          ]
          []
        , div [ ]
          [ div [ ]
            [ img [ src "github-star.svg" ] []
            , text " "
            , text <| toString total
            ]
          , div [ ]
            [ a [ href <| userUrl userName ] [ text userName ]
            ]
          ]
        ]
  in
    div [ class "avatar-list" ]
      (List.map renderRow mostStarsForRepos)
