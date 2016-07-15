module Bench.Array exposing (tiny, small, medium, large)

import Bench.Native exposing (Benchmark, BenchmarkSuite, bench, suite)
import CollectionsNg.Array as Array exposing (Array)


type alias Input =
    Array Int


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


build : Int -> () -> Input
build n =
    \() -> List.foldl (\i acc -> Array.push i acc) Array.empty [1..n]


set : Input -> () -> Input
set arr =
    \() -> Array.set 7 5 arr


push : Input -> () -> Input
push arr =
    \() -> Array.push 5 arr


get : Input -> () -> Maybe Int
get arr =
    \() -> Array.get 5 arr


slice : Int -> Int -> Input -> () -> Input
slice from to arr =
    \() -> Array.slice from to arr


map : Input -> () -> Input
map arr =
    \() -> Array.map ((+) 1) arr


equality : Input -> Input -> () -> Bool
equality a1 a2 =
    \() -> a1 == a2


createSuite : Int -> List Benchmark
createSuite n =
    let
        sampleArray =
            build n ()
    in
        [ bench "Build"
            <| build n
        , bench "Set"
            <| set sampleArray
        , bench "Push"
            <| push sampleArray
        , bench "Get"
            <| get sampleArray
        , bench "Slice from end"
            <| slice 0 -3 sampleArray
        , bench "Slice from both"
            <| slice 3 -3 sampleArray
        , bench "Mapping"
            <| map sampleArray
        , bench "Equality"
            <| equality sampleArray (Array.set 5 5 sampleArray)
        , bench "Equality fail"
            <| equality sampleArray (Array.set 5 7 sampleArray)
        , bench "Equality worst case"
            <| equality sampleArray (Array.foldl (\v acc -> Array.push v acc) Array.empty sampleArray)
        ]


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
