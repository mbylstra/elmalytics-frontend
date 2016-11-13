module Github exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

avatarUrl : String -> Int -> String
avatarUrl username size =
  "https://github.com/" ++ username ++ ".png?size=" ++ toString size


repoUrl : String -> String -> String
repoUrl userName repoName  =
  "https://github.com/" ++ userName ++ "/" ++ repoName


userUrl : String -> String
userUrl userName  =
  "https://github.com/" ++ userName



githubForkRibbon : String -> Html msg
githubForkRibbon url =
    a
        [ href url ]
        [ img
            [ alt "Fork me on GitHub"
            , src "https://camo.githubusercontent.com/365986a132ccd6a44c23a9169022c0b5c890c387/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f7265645f6161303030302e706e67"
            , attribute "style" "position: absolute; top: 0; right: 0; border: 0; z-index: 100"
            ]
            []
        ]

githubStarButton :
  { size : String
  , user : String
  , type' : String
  , repo: String
  , style : List (String, String)
  }
  -> Html msg
githubStarButton params =
    let
        url =
            "https://ghbtns.com/github-btn.html?user="
            ++ params.user
            ++ "&repo="
            ++ params.repo
            ++ "&type="
            ++ params.type'
            ++ "&count=true&size="
            ++ params.size
        iframeWidth = if params.size == "small" then 170 else 160
        iframeHeight = if params.size == "small" then 20 else 30
    in
        iframe
            [ attribute "frameborder" "0"
            , attribute "scrolling" "0"
            , width iframeWidth
            , height iframeHeight
            , src url
            , style params.style
            ]
            []
