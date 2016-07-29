module Main exposing (main)

import Html
import Html.App
import Bench.Native as Benchmark
import Bench.Array


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Done!"
        }
        |> Benchmark.run
            [ Bench.Array.large
            ]
