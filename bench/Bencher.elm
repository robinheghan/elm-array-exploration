module Main exposing (main)

import Html
import Html.App
import Bench.Native as Benchmark
import Bench.Dict
import Bench.Array


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Done!"
        }
        |> Benchmark.run
            [ --Bench.Dict.tiny
            --, Bench.Dict.small
            --, Bench.Dict.medium
            --, Bench.Dict.large
            Bench.Array.tiny
            , Bench.Array.small
            , Bench.Array.medium
            , Bench.Array.large
            ]
