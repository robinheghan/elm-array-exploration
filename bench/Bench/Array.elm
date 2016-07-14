module Bench.Array exposing (tiny, small, medium, large)

import Bench.Native exposing (Benchmark, BenchmarkSuite, bench, suite)
import CollectionsNg.Array as Array exposing (Array)


tinyArraySize : Int
tinyArraySize =
    10


smallArraySize : Int
smallArraySize =
    100


mediumArraySize : Int
mediumArraySize =
    5000


largeArraySize : Int
largeArraySize =
    10000


testArray : Array Int
testArray =
    insertBench largeArraySize ()


insertBench : Int -> (() -> Array Int)
insertBench n =
    \() -> List.foldl (\i acc -> Array.push i acc) Array.empty [1..n]


createSuite : Int -> List (Benchmark (Array Int))
createSuite n =
    [ bench "Insert" <| insertBench n ]


tiny : BenchmarkSuite
tiny =
    suite "Tiny Array" <| createSuite tinyArraySize


small : BenchmarkSuite
small =
    suite "Small Array" <| createSuite smallArraySize


medium : BenchmarkSuite
medium =
    suite "Medium Array" <| createSuite mediumArraySize


large : BenchmarkSuite
large =
    suite "Large Array" <| createSuite largeArraySize
