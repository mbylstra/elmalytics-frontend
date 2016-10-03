import Html.App
import Top exposing (init, view, update)



main =
  Html.App.beginnerProgram
    { model = init
    , update = update
    , view = view
    }
