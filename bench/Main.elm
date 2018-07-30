module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark, describe, benchmark1, benchmark2, benchmark3)
import Array.Hamt as Hamt
import Array


main : BenchmarkProgram
main =
    program <| suite 10000


suite : Int -> Benchmark
suite n =
    let
        sampleHamt =
            Hamt.initialize n identity

        sampleArray =
            Array.initialize n identity

        sampleList =
            List.range 1 n

        countFn acc _ =
            acc + 1

        isEven n =
            n % 2 == 0

        toListAndBack arr =
            arr
                |> Hamt.toList
                |> Hamt.fromList

        toSeqAndBack arr =
            arr
                |> Hamt.sequenceLeft
                |> Hamt.initializeFromSequencer
    in
        describe ("Array (" ++ toString n ++ " elements)")
            [ Benchmark.compare "intializer"
                (benchmark1 "List" toListAndBack sampleHamt)
                (benchmark1 "Seq" toSeqAndBack sampleHamt)
            , Benchmark.compare "intializer"
                (benchmark1 "Seq" toSeqAndBack sampleHamt)
                (benchmark1 "List" toListAndBack sampleHamt)

            {- Benchmark.compare "initialize"
                   (benchmark2 "Array" Array.initialize n identity)
                   (benchmark2 "HAMT" Hamt.initialize n identity)
               , Benchmark.compare "get"
                   (benchmark2 "Array" Array.get 5 sampleArray)
                   (benchmark2 "HAMT" Hamt.get 5 sampleHamt)
               , Benchmark.compare "set"
                   (benchmark3 "Array" Array.set 7 5 sampleArray)
                   (benchmark3 "HAMT" Hamt.set 7 5 sampleHamt)
               , Benchmark.compare "push"
                   (benchmark2 "Array" Array.push 5 sampleArray)
                   (benchmark2 "HAMT" Hamt.push 5 sampleHamt)
               , Benchmark.compare "append"
                   (benchmark2 "Array" Array.append sampleArray sampleArray)
                   (benchmark2 "HAMT" Hamt.append sampleHamt sampleHamt)
               , Benchmark.compare "append (small)"
                   (benchmark2 "Array" Array.append sampleArray (Array.initialize 31 identity))
                   (benchmark2 "HAMT" Hamt.append sampleHamt (Hamt.initialize 31 identity))
               , Benchmark.compare "slice (beginning, small)"
                   (benchmark3 "Array" Array.slice 3 n sampleArray)
                   (benchmark3 "HAMT" Hamt.slice 3 n sampleHamt)
               , Benchmark.compare "slice (beginning, big)"
                   (benchmark3 "Array" Array.slice (n // 2) n sampleArray)
                   (benchmark3 "HAMT" Hamt.slice (n // 2) n sampleHamt)
               , Benchmark.compare "slice (end, small)"
                   (benchmark3 "Array" Array.slice 0 -3 sampleArray)
                   (benchmark3 "HAMT" Hamt.slice 0 -3 sampleHamt)
               , Benchmark.compare "slice (end, big)"
                   (benchmark3 "Array" Array.slice 0 (n // 2) sampleArray)
                   (benchmark3 "HAMT" Hamt.slice 0 (n // 2) sampleHamt)
               , Benchmark.compare "foldl"
                   (benchmark3 "Array" Array.foldl countFn 0 sampleArray)
                   (benchmark3 "HAMT" Hamt.foldl countFn 0 sampleHamt)
               , Benchmark.compare "foldr"
                   (benchmark3 "Array" Array.foldr countFn 0 sampleArray)
                   (benchmark3 "HAMT" Hamt.foldr countFn 0 sampleHamt)
               , Benchmark.compare "filter"
                   (benchmark2 "Array" Array.filter isEven sampleArray)
                   (benchmark2 "HAMT" Hamt.filter isEven sampleHamt)
               , Benchmark.compare "map"
                   (benchmark2 "Array" Array.map identity sampleArray)
                   (benchmark2 "HAMT" Hamt.map identity sampleHamt)
               , Benchmark.compare "indexedMap"
                   (benchmark2 "Array" Array.indexedMap (,) sampleArray)
                   (benchmark2 "HAMT" Hamt.indexedMap (,) sampleHamt)
               , Benchmark.compare "toList"
                   (benchmark1 "Array" Array.toList sampleArray)
                   (benchmark1 "HAMT" Hamt.toList sampleHamt)
               , Benchmark.compare "fromList"
                   (benchmark1 "Array" Array.fromList sampleList)
                   (benchmark1 "HAMT" Hamt.fromList sampleList)
               , Benchmark.compare "indexedList"
                   (benchmark1 "Array" Array.toIndexedList sampleArray)
                   (benchmark1 "HAMT" Hamt.toIndexedList sampleHamt)
               , Benchmark.compare "= (equal, best case)"
                   (benchmark2 "Array" (==) sampleArray (Array.set 5 5 sampleArray))
                   (benchmark2 "HAMT" (==) sampleHamt (Hamt.set 5 5 sampleHamt))
               , Benchmark.compare "= (equal, worst case)"
                   (benchmark2 "Array" (==) sampleArray (Array.map identity sampleArray))
                   (benchmark2 "HAMT" (==) sampleHamt (Hamt.map identity sampleHamt))
               , Benchmark.compare "= (not equal)"
                   (benchmark2 "Array" (==) sampleArray (Array.set 5 7 sampleArray))
                   (benchmark2 "HAMT" (==) sampleHamt (Hamt.set 5 7 sampleHamt))
            -}
            ]
