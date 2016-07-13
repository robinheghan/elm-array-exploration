module Test.Dict exposing (tests)

import Test exposing (..)
import Expect
import CollectionsNg.Dict exposing (..)


tests : Test
tests =
    describe "Dict tests"
        [ emptyTests
        , sizeTests
        , getSetTests
        , conversionTests
        , mergeTests
        ]


simpleDict : Dict String String
simpleDict =
    fromList
        [ ( "Key1", "Val1" )
        , ( "Key2", "Val2" )
        , ( "Key3", "Val3" )
        ]


emptyTests : Test
emptyTests =
    describe "Empty"
        [ test "empty has 0 size"
            <| \() ->
                size empty
                    |> Expect.equal 0
        , test "0 size is empty"
            <| \() ->
                isEmpty empty
                    |> Expect.equal True
        ]


sizeTests : Test
sizeTests =
    describe "Length"
        [ test "simple count"
            <| \() ->
                size simpleDict
                    |> Expect.equal 3
        , test "insert increases size"
            <| \() ->
                size (insert "Key4" "Val4" simpleDict)
                    |> Expect.equal 4
        , test "replace does not increase size"
            <| \() ->
                size (insert "Key3" "Val45" simpleDict)
                    |> Expect.equal 3
        , test "remove decreases size"
            <| \() ->
                size (remove "Key2" simpleDict)
                    |> Expect.equal 2
        , test "remove noop does not decrease size"
            <| \() ->
                size (remove "Key50" simpleDict)
                    |> Expect.equal 3
        , test "singleton has 1 size"
            <| \() ->
                size (singleton "Key1" "Val1")
                    |> Expect.equal 1
        ]


getSetTests : Test
getSetTests =
    describe "Get set"
        [ test "get from singleton"
            <| \() ->
                get "Key1" (singleton "Key1" "Val1")
                    |> Expect.equal (Just "Val1")
        , test "simple get"
            <| \() ->
                get "Key2" simpleDict
                    |> Expect.equal (Just "Val2")
        , test "nonexistant get"
            <| \() ->
                get "Key4" simpleDict
                    |> Expect.equal Nothing
        , test "simple set"
            <| \() ->
                get "Key4" (insert "Key4" "Val4" simpleDict)
                    |> Expect.equal (Just "Val4")
        , test "simple remove"
            <| \() ->
                get "Key2" (remove "Key2" simpleDict)
                    |> Expect.equal Nothing
        , test "simple update"
            <| \() ->
                get "Key3" (update (always (Just "Val3Val")) "Key3" simpleDict)
                    |> Expect.equal (Just "Val3Val")
        , test "upsert"
            <| \() ->
                get "Key5" (update (always (Just "Val5")) "Key5" simpleDict)
                    |> Expect.equal (Just "Val5")
        , test "remove by update"
            <| \() ->
                get "Key2" (update (always Nothing) "Key2" simpleDict)
                    |> Expect.equal Nothing
        , test "member exists"
            <| \() ->
                member "Key2" simpleDict
                    |> Expect.equal True
        , test "member nonexist"
            <| \() ->
                member "Key4" simpleDict
                    |> Expect.equal False
        ]


conversionTests : Test
conversionTests =
    describe "Conversion"
        [ test "toList"
            <| \() ->
                simpleDict
                    |> Expect.equal (fromList [ ( "Key2", "Val2" ), ( "Key1", "Val1" ), ( "Key3", "Val3" ) ])
        , test "keys"
            <| \() ->
                keys simpleDict
                    |> Expect.equal [ "Key2", "Key1", "Key3" ]
        , test "values"
            <| \() ->
                values simpleDict
                    |> Expect.equal [ "Val2", "Val1", "Val3" ]
        ]


mergeTests : Test
mergeTests =
    let
        insertBoth key leftVal rightVal dict =
            insert key (leftVal ++ rightVal) dict

        s1 =
            singleton "u1" [ 1 ]

        s2 =
            singleton "u2" [ 2 ]

        s23 =
            singleton "u2" [ 3 ]

        b1 =
            fromList <| List.map (\i -> ( i, [ i ] )) [1..10]

        b2 =
            fromList <| List.map (\i -> ( i, [ i ] )) [5..15]

        bExpected =
            fromList [ ( 1, [ 1 ] ), ( 2, [ 2 ] ), ( 3, [ 3 ] ), ( 4, [ 4 ] ), ( 5, [ 5, 5 ] ), ( 6, [ 6, 6 ] ), ( 7, [ 7, 7 ] ), ( 8, [ 8, 8 ] ), ( 9, [ 9, 9 ] ), ( 10, [ 10, 10 ] ), ( 11, [ 11 ] ), ( 12, [ 12 ] ), ( 13, [ 13 ] ), ( 14, [ 14 ] ), ( 15, [ 15 ] ) ]
    in
        describe "merge Tests"
            [ test "merge empties"
                <| \() ->
                    merge insert insertBoth insert empty empty empty
                        |> Expect.equal empty
            , test "merge singletons in order"
                <| \() ->
                    merge insert insertBoth insert s1 s2 empty
                        |> Expect.equal (fromList [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ])
            , test "merge singletons out of order"
                <| \() ->
                    merge insert insertBoth insert s2 s1 empty
                        |> Expect.equal (fromList [ ( "u1", [ 1 ] ), ( "u2", [ 2 ] ) ])
            , test "merge with duplicate key"
                <| \() ->
                    merge insert insertBoth insert s2 s23 empty
                        |> Expect.equal (fromList [ ( "u2", [ 2, 3 ] ) ])
            , test "partially overlapping"
                <| \() ->
                    merge insert insertBoth insert b1 b2 empty
                        |> Expect.equal bExpected
            ]
