module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark)
import Native.JsArray


main : BenchmarkProgram
main =
    program <| suite


suite : Benchmark
suite =
    let
        ( arr, _ ) =
            Native.JsArray.initializeFromList 99 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

        fromArray arr =
            Native.JsArray.foldr (\item acc -> item :: acc) [] arr
    in
        Benchmark.describe "List representations"
            [ Benchmark.compare "Sum"
                "Literal"
                (\_ -> List.sum [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
                "Runtime conversion"
                (\_ -> List.sum (fromArray arr))
            ]
