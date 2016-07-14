module Bench.Dict exposing (tiny, small, medium, large)

import Bench.Native exposing (Benchmark, BenchmarkSuite, bench, suite)
import CollectionsNg.Dict as Dict exposing (Dict)


tinyDictSize : Int
tinyDictSize =
    10


smallDictSize : Int
smallDictSize =
    100


mediumDictSize : Int
mediumDictSize =
    5000


largeDictSize : Int
largeDictSize =
    10000


testKey : Int -> String
testKey n =
    "Key" ++ (toString n)


testDict : Dict String Int
testDict =
    insertBench largeDictSize ()


insertBench : Int -> (() -> Dict String Int)
insertBench n =
    \() -> List.foldl (\i acc -> Dict.insert (testKey i) i acc) Dict.empty [1..n]


createSuite : Int -> List (Benchmark (Dict String Int))
createSuite n =
    [ bench "Insert" <| insertBench n ]


tiny : BenchmarkSuite
tiny =
    suite "Tiny Dict" <| createSuite tinyDictSize


small : BenchmarkSuite
small =
    suite "Small Dict" <| createSuite smallDictSize


medium : BenchmarkSuite
medium =
    suite "Medium Dict" <| createSuite mediumDictSize


large : BenchmarkSuite
large =
    suite "Large Dict" <| createSuite largeDictSize
