module Test.Array exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import Hamt.Array exposing (..)


tests : Test
tests =
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
        [ test "initialize" <|
            \() ->
                toList (initialize 4 identity)
                    |> Expect.equal [ 0, 1, 2, 3 ]
        , test "initialize second" <|
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
        , test "repeat" <|
            \() ->
                toList (repeat 3 0)
                    |> Expect.equal [ 0, 0, 0 ]
        , test "Large build" <|
            \() ->
                toList (initialize 40000 identity)
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
        , test "array of one" <|
            \() ->
                length (fromList [ 1 ])
                    |> Expect.equal 1
        , test "array of two" <|
            \() ->
                length (fromList [ 1, 2 ])
                    |> Expect.equal 2
        , test "large array" <|
            \() ->
                length (fromList [1..60])
                    |> Expect.equal 60
        , test "push" <|
            \() ->
                length (push 3 (fromList [ 1, 2 ]))
                    |> Expect.equal 3
        , test "append" <|
            \() ->
                length (append (fromList [ 1, 2 ]) (fromList [ 3, 4, 5 ]))
                    |> Expect.equal 5
        , test "set does not increase" <|
            \() ->
                length (set 1 1 (fromList [ 1, 2, 3 ]))
                    |> Expect.equal 3
        , test "slice" <|
            \() ->
                length (slice 35 -35 (fromList [0..1063]))
                    |> Expect.equal 994
        , test "small slice end" <|
            \() ->
                length (slice 0 -3 (fromList [0..9]))
                    |> Expect.equal 7
        , test "bigger slice end" <|
            \() ->
                length (slice 0 -35 (fromList [0..69]))
                    |> Expect.equal 35
        ]


equality : Test
equality =
    describe "Equality"
        [ test "don't matter how you build" <|
            \() ->
                initialize 5 identity
                    |> Expect.equal (List.foldl push empty [0..4])
        , test "small slice" <|
            \() ->
                slice 3 -3 (fromList [0..1063])
                    |> Expect.equal (fromList [3..1060])
        , test "large slice" <|
            \() ->
                slice 35 -35 (fromList [0..1063])
                    |> Expect.equal (fromList [35..1028])
        ]


getSet : Test
getSet =
    describe "Get and set"
        [ test "can retrieve element" <|
            \() ->
                get 1 (fromList [ 1, 2, 3 ])
                    |> Expect.equal (Just 2)
        , test "can retrieve element in large array" <|
            \() ->
                get 45 (fromList [0..65])
                    |> Expect.equal (Just 45)
        , test "out of bounds retrieval returns nothing" <|
            \() ->
                get 1 (fromList [ 1 ])
                    |> Expect.equal Nothing
        , test "set replaces value" <|
            \() ->
                set 1 5 (fromList [ 1, 2, 3 ])
                    |> Expect.equal (fromList [ 1, 5, 3 ])
        , test "set out of bounds returns original array" <|
            \() ->
                set 3 5 (fromList [ 1, 2, 3 ])
                    |> Expect.equal (fromList [ 1, 2, 3 ])
        , test "can retrieve from tail" <|
            \() ->
                get 1026 (fromList [0..1030])
                    |> Expect.equal (Just 1026)
        , test "can retrieve from tree" <|
            \() ->
                get 1022 (fromList [0..1030])
                    |> Expect.equal (Just 1022)
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
            ]


runtimeCrash : Test
runtimeCrash =
    describe "Runtime crashes in core"
        [ {- test "magic slice" <|
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
             ,
          -}
          test "magic slice 2" <|
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
