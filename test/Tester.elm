module Main exposing (..)

import Html
import Html.App
import Test exposing (..)
import Test.Runner.Log as Log
import Test.Hamt as Hamt
import Test.Array as Array
import Test.Dict as Dict


tests : List Test
tests =
    [ Hamt.tests
    , Array.tests
    , Dict.tests
    ]


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Check the console for useful output!"
        }
        |> Log.run (concat tests)
