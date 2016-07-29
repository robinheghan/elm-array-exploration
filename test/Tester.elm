module Main exposing (..)

import Html
import Html.App
import Test.Runner.Log as Log
import Test.Array as Array


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Check the console for useful output!"
        }
        |> Log.run Array.tests
