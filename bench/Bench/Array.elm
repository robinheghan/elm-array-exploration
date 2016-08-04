module Bench.Array exposing (large)

import Bench.Native exposing (Benchmark, BenchmarkSuite, bench, suite)
import Hamt.Array as Array exposing (Array)


type alias Input =
    Array Int


largeArraySize : Int
largeArraySize =
    10000


buildByPush : Int -> () -> Input
buildByPush n =
    \() -> List.foldl Array.push Array.empty [1..n]


buildByInitialize : Int -> () -> Input
buildByInitialize n =
    \() -> Array.initialize n identity


set : Input -> () -> Input
set arr =
    \() -> Array.set 7 5 arr


push : Input -> () -> Input
push arr =
    \() -> Array.push 5 arr


get : Input -> () -> Maybe Int
get arr =
    \() -> Array.get 5 arr


append : Input -> Input -> () -> Input
append a1 a2 =
    \() -> Array.append a1 a2


slice : Int -> Int -> Input -> () -> Input
slice from to arr =
    \() -> Array.slice from to arr


fold : Input -> () -> Int
fold arr =
    \() -> Array.foldl (\_ acc -> acc + 1) 0 arr


map : Input -> () -> Array.Array Int
map arr =
    \() -> Array.map identity arr


indexedMap : Input -> () -> Array.Array ( Int, Int )
indexedMap arr =
    \() -> Array.indexedMap (,) arr


indexedList : Input -> () -> List ( Int, Int )
indexedList arr =
    \() -> Array.toIndexedList arr


equality : Input -> Input -> () -> Bool
equality a1 a2 =
    \() -> a1 == a2


createSuite : Int -> List Benchmark
createSuite n =
    let
        sampleArray =
            buildByPush n ()
    in
        [ bench "Build by push" <|
            buildByPush n
        , bench "Build by initialize" <|
            buildByInitialize n
        , bench "Set" <|
            set sampleArray
        , bench "Push" <|
            push sampleArray
        , bench "Get" <|
            get sampleArray
        , bench "Append" <|
            append sampleArray sampleArray
        , bench "Slice from beginning minor" <|
            slice 3 n sampleArray
        , bench "Slice from beginning mayor" <|
            slice (n // 2) n sampleArray
        , bench "Slice from end minor" <|
            slice 0 -3 sampleArray
        , bench "Slice from end mayor" <|
            slice 0 (n // 2) sampleArray
        , bench "Slice from both minor" <|
            slice 3 -3 sampleArray
        , bench "Slice from both mayor" <|
            slice ((n // 2) - 10) (n // 2) sampleArray
        , bench "Fold" <|
            fold sampleArray
        , bench "Map" <|
            map sampleArray
        , bench "Indexed Map" <|
            indexedMap sampleArray
        , bench "Indexed List" <|
            indexedList sampleArray
        , bench "Equality" <|
            equality sampleArray (Array.set 5 5 sampleArray)
        , bench "Equality fail" <|
            equality sampleArray (Array.set 5 7 sampleArray)
        , bench "Equality worst case" <|
            equality sampleArray (Array.foldl (\v acc -> Array.push v acc) Array.empty sampleArray)
        ]


large : BenchmarkSuite
large =
    suite
        ("Array ("
            ++ (toString largeArraySize)
            ++ " elements)"
        )
        (createSuite largeArraySize)
