module Tests exposing (all)

import Test exposing (Test, describe, test, fuzz, fuzz2)
import Fuzz exposing (intRange)
import Expect
import Hamt.Array exposing (..)


all : Test
all =
    describe "Array"
        [ init'
        , isEmpty'
        , length'
        , equality
        , getSet
        , conversion
        , transform
        , slice'
        , runtimeCrash
        ]


init' : Test
init' =
    describe "Initialization"
        [ fuzz (intRange 1 10000) "initialize" <|
            \size ->
                toList (initialize size identity)
                    |> Expect.equal [0..(size - 1)]
        , test "initialize non-identity" <|
            \() ->
                toList (initialize 4 (\n -> n * n))
                    |> Expect.equal [ 0, 1, 4, 9 ]
        , test "initialize empty" <|
            \() ->
                toList (initialize 0 identity)
                    |> Expect.equal []
        , test "initialize negative" <|
            \() ->
                toList (initialize -2 identity)
                    |> Expect.equal []
        , test "Large initialize" <|
            \() ->
                toList (initialize 40000 identity)
                    |> Expect.equal [0..39999]
        , test "Large push" <|
            \() ->
                toList (List.foldl push empty [0..39999])
                    |> Expect.equal [0..39999]
        ]


isEmpty' : Test
isEmpty' =
    describe "isEmpty"
        [ test "all empty arrays are equal" <|
            \() ->
                Expect.equal empty (fromList [])
        , test "empty array" <|
            \() ->
                isEmpty empty
                    |> Expect.equal True
        , test "empty converted array" <|
            \() ->
                isEmpty (fromList [])
                    |> Expect.equal True
        , test "non-empty array" <|
            \() ->
                isEmpty (fromList [ 1 ])
                    |> Expect.equal False
        ]


length' : Test
length' =
    describe "Length"
        [ test "empty array" <|
            \() ->
                length empty
                    |> Expect.equal 0
        , fuzz (intRange 1 10000) "non-empty array" <|
            \size ->
                length (initialize size identity)
                    |> Expect.equal size
        , fuzz (intRange 1 10000) "push" <|
            \size ->
                length (push size (initialize size identity))
                    |> Expect.equal (size + 1)
        , fuzz (intRange 1 1000) "append" <|
            \size ->
                length (append (initialize size identity) (initialize (size // 2) identity))
                    |> Expect.equal (size + (size // 2))
        , fuzz (intRange 1 10000) "set does not increase" <|
            \size ->
                length (set (size // 2) 1 (initialize size identity))
                    |> Expect.equal size
        , fuzz (intRange 100 10000) "big slice" <|
            \size ->
                length (slice 35 -35 (initialize size identity))
                    |> Expect.equal (size - 70)
        , fuzz2 (intRange -32 0) (intRange 100 10000) "small slice end" <|
            \n size ->
                length (slice 0 n (initialize size identity))
                    |> Expect.equal (size + n)
        ]


equality : Test
equality =
    describe "Equality"
        [ fuzz (intRange 1 10000) "don't matter how you build" <|
            \size ->
                initialize size identity
                    |> Expect.equal (List.foldl push empty [0..(size - 1)])
        , fuzz2 (intRange -35 0) (intRange 100 35000) "slice" <|
            \n size ->
                slice (abs n) n (initialize size identity)
                    |> Expect.equal (initialize (size + n + n) (\idx -> idx - n))
        ]


getSet : Test
getSet =
    describe "Get and set"
        [ fuzz2 (intRange 1 35000) (intRange 1 35000) "can retrieve element" <|
            \x y ->
                let
                    n =
                        min x y

                    size =
                        max x y
                in
                    get n (initialize size identity)
                        |> Expect.equal (Just n)
        , test "out of bounds retrieval returns nothing" <|
            \() ->
                get 1 (fromList [ 1 ])
                    |> Expect.equal Nothing
        , fuzz2 (intRange 1 35000) (intRange 1 35000) "set replaces value" <|
            \x y ->
                let
                    n =
                        min x y

                    size =
                        max x y
                in
                    get n (set n 5 (initialize size identity))
                        |> Expect.equal (Just 5)
        , fuzz (intRange 1 35000) "set out of bounds returns original array" <|
            \size ->
                let
                    arr =
                        initialize size identity
                in
                    set -1 5 arr
                        |> set (size + 1) 5
                        |> Expect.equal arr
        ]


conversion : Test
conversion =
    describe "Conversion"
        [ test "empty array" <|
            \() ->
                toList (fromList [])
                    |> Expect.equal []
        , test "correct element" <|
            \() ->
                toList (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ 1, 2, 3 ]
        , test "indexed" <|
            \() ->
                toIndexedList (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]
        ]


transform : Test
transform =
    describe "Transform"
        [ test "foldl" <|
            \() ->
                foldl (::) [] (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ 3, 2, 1 ]
        , test "foldr" <|
            \() ->
                foldr (\n acc -> n :: acc) [] (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ 1, 2, 3 ]
        , test "filter" <|
            \() ->
                toList (filter (\a -> a % 2 == 0) (fromList [1..6]))
                    |> Expect.equal [ 2, 4, 6 ]
        , test "map" <|
            \() ->
                toList (map ((+) 1) (fromList [ 1, 2, 3 ]))
                    |> Expect.equal [ 2, 3, 4 ]
        , test "indexedMap" <|
            \() ->
                toList (indexedMap (*) (fromList [ 5, 5, 5 ]))
                    |> Expect.equal [ 0, 5, 10 ]
        , test "push appends one element" <|
            \() ->
                toList (push 3 (fromList [ 1, 2 ]))
                    |> Expect.equal [ 1, 2, 3 ]
        , test "append" <|
            \() ->
                toList (append (fromList [1..60]) (fromList [61..120]))
                    |> Expect.equal [1..120]
        ]


slice' : Test
slice' =
    let
        smallSample =
            fromList [1..8]

        largeSample =
            fromList [0..1063]

        largeLen =
            1064
    in
        describe "Slice"
            [ test "both small" <|
                \() ->
                    toList (slice 2 5 smallSample)
                        |> Expect.equal [3..5]
            , test "start small" <|
                \() ->
                    let
                        arr =
                            fromList [1..10]
                    in
                        toList (slice 2 (length arr) arr)
                            |> Expect.equal [3..10]
            , test "negative" <|
                \() ->
                    toList (slice -5 -2 smallSample)
                        |> Expect.equal [4..6]
            , test "combined" <|
                \() ->
                    toList (slice 2 -2 smallSample)
                        |> Expect.equal [3..6]
            , test "impossible" <|
                \() ->
                    toList (slice -1 -2 smallSample)
                        |> Expect.equal []
            , test "start mayor" <|
                \() ->
                    toList (slice (largeLen // 2) largeLen largeSample)
                        |> Expect.equal [532..1063]
            , test "end mayor" <|
                \() ->
                    toList (slice 0 (largeLen // 2) largeSample)
                        |> Expect.equal [0..531]
            , test "both mayor" <|
                \() ->
                    toList (slice (largeLen // 2) (largeLen // 2 + 10) largeSample)
                        |> Expect.equal [532..541]
            , test "start minor" <|
                \() ->
                    toList (slice 3 largeLen largeSample)
                        |> Expect.equal [3..1063]
            , test "end minor" <|
                \() ->
                    toList (slice 0 -3 largeSample)
                        |> Expect.equal [0..1060]
            , test "both minor" <|
                \() ->
                    toList (slice 3 -3 largeSample)
                        |> Expect.equal [3..1060]
            , test "past threshold left" <|
                \() ->
                    slice 60 largeLen largeSample
                        |> Expect.equal (fromList [60..(largeLen - 1)])
            , test "past threshold right" <|
                \() ->
                    slice 0 -60 largeSample
                        |> Expect.equal (fromList [0..(largeLen - 61)])
            , test "reduce to single element" <|
                \() ->
                    slice 0 1 largeSample
                        |> Expect.equal (fromList [ 0 ])
            ]


runtimeCrash : Test
runtimeCrash =
    describe "Runtime crashes in core"
        [ test "magic slice" <|
            \() ->
                let
                    n =
                        10
                in
                    initialize (4 * n) identity
                        |> slice n (4 * n)
                        |> slice n (3 * n)
                        |> slice n (2 * n)
                        |> slice n n
                        |> \a -> Expect.equal a a
        , test "magic slice 2" <|
            \() ->
                let
                    ary =
                        fromList [0..32]

                    res =
                        append (slice 1 32 ary) (slice (32 + 1) -1 ary)
                in
                    Expect.equal res res
        , test "magic append" <|
            \() ->
                let
                    res =
                        append (initialize 1 (always 1))
                            (initialize (32 ^ 2 - 1 * 32 + 1) (\i -> i))
                in
                    Expect.equal res res
        ]
