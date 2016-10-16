module Main exposing (main)

import Html
import Bench.Native as Benchmark
import Bench.Array


main : Program Never () a
main =
    Html.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Done!"
        }
        |> Benchmark.run
            [ Bench.Array.large
            ]
