module Tests exposing (all)

import Test exposing (Test, describe, test, fuzz, fuzz2)
import Fuzz exposing (Fuzzer, intRange)
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


{-| > 33000 elements requires 3 levels in the tree
-}
defaultSizeRange : Fuzzer Int
defaultSizeRange =
    (intRange 1 35000)


init' : Test
init' =
    describe "Initialization"
        [ fuzz defaultSizeRange "initialize" <|
            \size ->
                toList (initialize size identity)
                    |> Expect.equal [0..(size - 1)]
        , fuzz defaultSizeRange "push" <|
            \size ->
                List.foldl push empty [0..(size - 1)]
                    |> Expect.equal (initialize size identity)
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
        , fuzz defaultSizeRange "non-empty array" <|
            \size ->
                length (initialize size identity)
                    |> Expect.equal size
        , fuzz defaultSizeRange "push" <|
            \size ->
                length (push size (initialize size identity))
                    |> Expect.equal (size + 1)
        , fuzz defaultSizeRange "append" <|
            \size ->
                length (append (initialize size identity) (initialize (size // 2) identity))
                    |> Expect.equal (size + (size // 2))
        , fuzz defaultSizeRange "set does not increase" <|
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
        [ fuzz2 (intRange -35 0) (intRange 100 35000) "slice" <|
            \n size ->
                slice (abs n) n (initialize size identity)
                    |> Expect.equal (initialize (size + n + n) (\idx -> idx - n))
        ]


getSet : Test
getSet =
    describe "Get and set"
        [ fuzz2 defaultSizeRange defaultSizeRange "can retrieve element" <|
            \x y ->
                let
                    n =
                        min x y

                    size =
                        max x y
                in
                    get n (initialize size identity)
                        |> Expect.equal (Just n)
        , fuzz2 (intRange 1 50) (intRange 100 35000) "out of bounds retrieval returns nothing" <|
            \n size ->
                let
                    arr =
                        initialize size identity
                in
                    ( get (negate n) arr
                    , get (size + n) arr
                    )
                        |> Expect.equal ( Nothing, Nothing )
        , fuzz2 defaultSizeRange defaultSizeRange "set replaces value" <|
            \x y ->
                let
                    n =
                        min x y

                    size =
                        max x y
                in
                    get n (set n 5 (initialize size identity))
                        |> Expect.equal (Just 5)
        , fuzz2 (intRange 0 50) defaultSizeRange "set out of bounds returns original array" <|
            \n size ->
                let
                    arr =
                        initialize size identity
                in
                    set (negate n) 5 arr
                        |> set (size + n) 5
                        |> Expect.equal arr
        ]


conversion : Test
conversion =
    describe "Conversion"
        [ fuzz defaultSizeRange "back and forth" <|
            \size ->
                let
                    ls =
                        [0..(size - 1)]
                in
                    toList (fromList ls)
                        |> Expect.equal ls
        , fuzz defaultSizeRange "indexed" <|
            \size ->
                toIndexedList (initialize size ((+) 1))
                    |> Expect.equal (toList (initialize size (\idx -> ( idx, idx + 1 ))))
        ]


transform : Test
transform =
    describe "Transform"
        [ fuzz defaultSizeRange "foldl" <|
            \size ->
                foldl (::) [] (initialize size identity)
                    |> Expect.equal (List.reverse [0..(size - 1)])
        , fuzz defaultSizeRange "foldr" <|
            \size ->
                foldr (\n acc -> n :: acc) [] (initialize size identity)
                    |> Expect.equal [0..(size - 1)]
        , fuzz defaultSizeRange "filter" <|
            \size ->
                toList (filter (\a -> a % 2 == 0) (initialize size identity))
                    |> Expect.equal (List.filter (\a -> a % 2 == 0) [0..(size - 1)])
        , fuzz defaultSizeRange "map" <|
            \size ->
                map ((+) 1) (initialize size identity)
                    |> Expect.equal (initialize size ((+) 1))
        , fuzz defaultSizeRange "indexedMap" <|
            \size ->
                indexedMap (*) (repeat size 5)
                    |> Expect.equal (initialize size ((*) 5))
        , fuzz defaultSizeRange "push appends one element" <|
            \size ->
                push size (initialize size identity)
                    |> Expect.equal (initialize (size + 1) identity)
        , fuzz (intRange 1 1050) "append" <|
            \size ->
                append (initialize size identity) (initialize size (\idx -> idx + size))
                    |> Expect.equal (initialize (size * 2) identity)
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
