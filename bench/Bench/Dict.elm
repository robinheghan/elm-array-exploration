module Bench.Dict exposing (tiny, small, medium, large)

import Bench.Native exposing (Benchmark, BenchmarkSuite, bench, suite)
import CollectionsNg.Dict as Dict exposing (Dict)


type alias Input =
    Dict String Int


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


testEntries : List ( String, Int )
testEntries =
    List.foldl (\i acc -> ( testKey i, i ) :: acc) [] [1..largeDictSize]


testKey : Int -> String
testKey i =
    "DictKey" ++ (toString i)


insert : List ( String, Int ) -> (() -> Input)
insert entries =
    \() -> List.foldl (\( k, v ) acc -> Dict.insert k v acc) Dict.empty entries


remove : List String -> Input -> (() -> Input)
remove keys dict =
    \() -> List.foldl (\key acc -> Dict.remove key acc) dict keys


set : Input -> (() -> Input)
set dict =
    \() -> Dict.insert (testKey 5) 10 dict


lookup : Input -> () -> Maybe Int
lookup dict =
    \() -> Dict.get (testKey 5) dict


lookupFail : Input -> () -> Maybe Int
lookupFail dict =
    \() -> Dict.get (testKey 20000) dict


update : Input -> (() -> Input)
update dict =
    \() -> Dict.update (testKey 5) (\v -> Just 10) dict


equality : Input -> Input -> (() -> Bool)
equality d1 d2 =
    \() -> d1 == d2


fold : Input -> () -> Int
fold dict =
    \() -> Dict.foldl (\k v acc -> acc + 1) 0 dict


createSuite : List ( String, Int ) -> List Benchmark
createSuite entries =
    let
        sampleDict =
            insert entries ()
    in
        [ bench "Insert"
            <| insert entries
        , bench "Remove"
            <| remove (List.map fst entries) sampleDict
        , bench "Set"
            <| set sampleDict
        , bench "Lookup"
            <| lookup sampleDict
        , bench "Lookup fail"
            <| lookupFail sampleDict
        , bench "Update"
            <| update sampleDict
        , bench "Equality"
            <| equality sampleDict (Dict.insert (testKey 5) 5 sampleDict)
        , bench "Equality fail"
            <| equality sampleDict (Dict.insert (testKey 5) 10 sampleDict)
        , bench "Equality worst case"
            <| equality sampleDict (Dict.foldl (\k v acc -> Dict.insert k v acc) Dict.empty sampleDict)
        , bench "Fold"
            <| fold sampleDict
        ]


tiny : BenchmarkSuite
tiny =
    suite "Tiny Dict" <| createSuite <| List.take tinyDictSize testEntries


small : BenchmarkSuite
small =
    suite "Small Dict" <| createSuite <| List.take smallDictSize testEntries


medium : BenchmarkSuite
medium =
    suite "Medium Dict" <| createSuite <| List.take mediumDictSize testEntries


large : BenchmarkSuite
large =
    suite "Large Dict" <| createSuite testEntries
