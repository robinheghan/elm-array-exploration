module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark)
import Array.Hamt as Array
import Native.JsArray


main : BenchmarkProgram
main =
    program <| suite


suite : Benchmark
suite =
    let
        hamt =
            Array.fromList [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

        ( arr, _ ) =
            Native.JsArray.initializeFromList 99 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

        fromArray arr =
            Native.JsArray.foldr (\item acc -> item :: acc) [] arr
    in
        Benchmark.describe "Literal representations"
            [ Benchmark.compare "List Sum"
                "Literal"
                (\_ -> List.foldl (\a b -> a + b) 0 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
                "Runtime conversion"
                (\_ -> List.foldl (\a b -> a + b) 0 (fromArray arr))
            , Benchmark.compare "Literal Sum"
                "Array"
                -- For an accurate comparison, edit the compiled index.html
                -- replace 'hamt' with '{ctor:'Array',_0:9,_1:5,_2:[],_3:[1,2,3,4,5,6,7,8,9]}'
                (\_ -> Array.foldl (\a b -> a + b) 0 hamt)
                "List"
                (\_ -> List.foldl (\a b -> a + b) 0 [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])
            , Benchmark.compare "Runtime Sum"
                "Array"
                -- For an accurate comparison, edit the compiled index.html
                -- replace 'hamt' with '{ctor:'Array',_0:9,_1:5,_2:[],_3:[1,2,3,4,5,6,7,8,9]}'
                (\_ -> Array.foldl (\a b -> a + b) 0 hamt)
                "List"
                (\_ -> List.foldl (\a b -> a + b) 0 (fromArray arr))
            ]
