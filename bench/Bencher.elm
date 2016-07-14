module Main exposing (..)

import Html
import Html.App
import Bench.Native as Benchmark


main : Program Never
main =
    Html.App.beginnerProgram
        { model = ()
        , update = \_ _ -> ()
        , view = \() -> Html.text "Check the console for output."
        }
        |> Benchmark.run [ Benchmark.suite "Test" [ Benchmark.bench "Test1" (\() -> "Test" ++ "Best") ] ]
