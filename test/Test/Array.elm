module Test.Array exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import CollectionsNg.Array exposing (..)


tests : Test
tests =
    describe "Array"
        [ init'
        , isEmpty'
        , length'
        , getSet
        , conversion
        , stack
        , transform
        ]


init' : Test
init' =
    describe "Initialization"
        [ test "initialize"
            <| \() ->
                toList (initialize 4 identity)
                    |> Expect.equal [ 0, 1, 2, 3 ]
        , test "initialize second"
            <| \() ->
                toList (initialize 4 (\n -> n * n))
                    |> Expect.equal [ 0, 1, 4, 9 ]
        , test "initialize empty"
            <| \() ->
                toList (initialize 0 identity)
                    |> Expect.equal []
        , test "initialize negative"
            <| \() ->
                toList (initialize -2 identity)
                    |> Expect.equal []
        , test "repeat"
            <| \() ->
                toList (repeat 3 0)
                    |> Expect.equal [ 0, 0, 0 ]
        ]


isEmpty' : Test
isEmpty' =
    describe "isEmpty"
        [ test "all empty arrays are equal"
            <| \() ->
                Expect.equal empty (fromList [])
        , test "empty array"
            <| \() ->
                isEmpty empty
                    |> Expect.equal True
        , test "empty converted array"
            <| \() ->
                isEmpty (fromList [])
                    |> Expect.equal True
        , test "non-empty array"
            <| \() ->
                isEmpty (fromList [ 1 ])
                    |> Expect.equal False
        ]


length' : Test
length' =
    describe "Length"
        [ test "empty array"
            <| \() ->
                length empty
                    |> Expect.equal 0
        , test "array of one"
            <| \() ->
                length (fromList [ 1 ])
                    |> Expect.equal 1
        , test "array of two"
            <| \() ->
                length (fromList [ 1, 2 ])
                    |> Expect.equal 2
        , test "large array"
            <| \() ->
                length (fromList [1..60])
                    |> Expect.equal 60
        , test "push"
            <| \() ->
                length (push 3 (fromList [ 1, 2 ]))
                    |> Expect.equal 3
        , test "pop"
            <| \() ->
                length (pop (fromList [ 1, 2, 3 ]))
                    |> Expect.equal 2
        , test "append"
            <| \() ->
                length (append (fromList [ 1, 2 ]) (fromList [ 3, 4, 5 ]))
                    |> Expect.equal 5
        , test "set does not increase"
            <| \() ->
                length (set 1 1 (fromList [ 1, 2, 3 ]))
                    |> Expect.equal 3
        ]


getSet : Test
getSet =
    describe "Get and set"
        [ test "can retrieve element"
            <| \() ->
                get 1 (fromList [ 1, 2, 3 ])
                    |> Expect.equal (Just 2)
        , test "can retrieve element in large array"
            <| \() ->
                get 45 (fromList [0..65])
                    |> Expect.equal (Just 45)
        , test "out of bounds retrieval returns nothing"
            <| \() ->
                get 1 (fromList [ 1 ])
                    |> Expect.equal Nothing
        , test "set replaces value"
            <| \() ->
                set 1 5 (fromList [ 1, 2, 3 ])
                    |> Expect.equal (fromList [ 1, 5, 3 ])
        , test "set out of bounds returns original array"
            <| \() ->
                set 3 5 (fromList [ 1, 2, 3 ])
                    |> Expect.equal (fromList [ 1, 2, 3 ])
        ]


conversion : Test
conversion =
    describe "Conversion"
        [ test "empty array"
            <| \() ->
                toList (fromList [])
                    |> Expect.equal []
        , test "correct element"
            <| \() ->
                toList (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ 1, 2, 3 ]
        , test "indexed"
            <| \() ->
                toIndexedList (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ]
        ]


stack : Test
stack =
    describe "Stack"
        [ test "pop empty array returns empty array"
            <| \() ->
                pop empty
                    |> Expect.equal empty
        , test "pop removes last element"
            <| \() ->
                toList (pop (fromList [ 1, 2, 3 ]))
                    |> Expect.equal [ 1, 2 ]
        , test "push appends one element"
            <| \() ->
                toList (push 3 (fromList [ 1, 2 ]))
                    |> Expect.equal [ 1, 2, 3 ]
        ]


transform : Test
transform =
    describe "Transform"
        [ test "foldl"
            <| \() ->
                foldl (::) [] (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ 3, 2, 1 ]
        , test "foldr"
            <| \() ->
                foldr (\n acc -> n :: acc) [] (fromList [ 1, 2, 3 ])
                    |> Expect.equal [ 1, 2, 3 ]
        , test "filter"
            <| \() ->
                toList (filter (\a -> a % 2 == 0) (fromList [1..6]))
                    |> Expect.equal [ 2, 4, 6 ]
        , test "map"
            <| \() ->
                toList (map ((+) 1) (fromList [ 1, 2, 3 ]))
                    |> Expect.equal [ 2, 3, 4 ]
        , test "indexedMap"
            <| \() ->
                toList (indexedMap (*) (fromList [ 5, 5, 5 ]))
                    |> Expect.equal [ 0, 5, 10 ]
        , test "append"
            <| \() ->
                toList (append (fromList [1..60]) (fromList [61..120]))
                    |> Expect.equal [1..120]
        , test "slice"
            <| \() ->
                toList (slice 2 5 (fromList [1..8]))
                    |> Expect.equal [3..5]
        , test "negative slice"
            <| \() ->
                toList (slice -5 -2 (fromList [1..8]))
                    |> Expect.equal [4..6]
        , test "combined slice"
            <| \() ->
                toList (slice 2 -2 (fromList [1..8]))
                    |> Expect.equal [3..6]
        , test "impossible slice"
            <| \() ->
                toList (slice 6 -2 (fromList [1..8]))
                    |> Expect.equal []
        ]
