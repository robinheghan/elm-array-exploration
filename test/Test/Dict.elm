module Test.Dict exposing (tests)

import Test exposing (..)
import Expect
import CollectionsNg.Dict as Dict exposing (..)


tests : Test
tests =
    describe "Dict"
        [ empty'
        , size'
        , transform
        , query
        , conversion'
        , combine
        , merge'
        ]


simpleDict : Dict String String
simpleDict =
    fromList
        [ ( "Key1", "Val1" )
        , ( "Key2", "Val2" )
        , ( "Key3", "Val3" )
        ]


empty' : Test
empty' =
    describe "empty"
        [ test "all empty dicts are equal"
            <| \() ->
                Expect.equal empty (fromList [])
        , test "empty dict has 0 size"
            <| \() ->
                size empty
                    |> Expect.equal 0
        , test "a dict with 0 size is empty"
            <| \() ->
                isEmpty empty
                    |> Expect.equal True
        ]


size' : Test
size' =
    describe "size"
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


transform : Test
transform =
    describe "Transform"
        [ test "simple set"
            <| \() ->
                get "Key4" (insert "Key4" "Val4" simpleDict)
                    |> Expect.equal (Just "Val4")
        , test "simple remove"
            <| \() ->
                get "Key2" (remove "Key2" simpleDict)
                    |> Expect.equal Nothing
        , test "simple update"
            <| \() ->
                get "Key3" (update "Key3" (always (Just "Val3Val")) simpleDict)
                    |> Expect.equal (Just "Val3Val")
        , test "upsert"
            <| \() ->
                get "Key5" (update "Key5" (always (Just "Val5")) simpleDict)
                    |> Expect.equal (Just "Val5")
        , test "remove by update"
            <| \() ->
                get "Key2" (update "Key2" (always Nothing) simpleDict)
                    |> Expect.equal Nothing
        , test "remove nothing"
            <| \() ->
                Expect.equal (singleton "k" "v") (remove "kk" (singleton "k" "v"))
        , test "filter"
            <| \() ->
                Expect.equal (singleton "Tom" "cat")
                    (Dict.filter (\k v -> k == "Tom") animals)
        , test "partition"
            <| \() ->
                Expect.equal ( singleton "Tom" "cat", singleton "Jerry" "mouse" )
                    (partition (\k v -> k == "Tom") animals)
        ]


query : Test
query =
    describe "Query"
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
        , test "member exists"
            <| \() ->
                member "Key2" simpleDict
                    |> Expect.equal True
        , test "member nonexist"
            <| \() ->
                member "Key4" simpleDict
                    |> Expect.equal False
        ]


conversion' : Test
conversion' =
    describe "Conversion"
        [ test "toList"
            <| \() ->
                simpleDict
                    |> Expect.equal (fromList [ ( "Key2", "Val2" ), ( "Key1", "Val1" ), ( "Key3", "Val3" ) ])
        , test "keys"
            <| \() ->
                List.sort (keys simpleDict)
                    |> Expect.equal [ "Key1", "Key2", "Key3" ]
        , test "values"
            <| \() ->
                List.sort (values simpleDict)
                    |> Expect.equal [ "Val1", "Val2", "Val3" ]
        ]


animals : Dict String String
animals =
    fromList [ ( "Tom", "cat" ), ( "Jerry", "mouse" ) ]


combine : Test
combine =
    describe "combine"
        [ test "union"
            <| \() -> Expect.equal animals (union (singleton "Jerry" "mouse") (singleton "Tom" "cat"))
        , test "union collison"
            <| \() -> Expect.equal (singleton "Tom" "cat") (union (singleton "Tom" "cat") (singleton "Tom" "mouse"))
        , test "intersect"
            <| \() -> Expect.equal (singleton "Tom" "cat") (intersect animals (singleton "Tom" "cat"))
        , test "diff"
            <| \() -> Expect.equal (singleton "Jerry" "mouse") (diff animals (singleton "Tom" "cat"))
        ]


merge' : Test
merge' =
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
            fromList
                [ ( 1, [ 1 ] )
                , ( 2, [ 2 ] )
                , ( 3, [ 3 ] )
                , ( 4, [ 4 ] )
                , ( 5, [ 5, 5 ] )
                , ( 6, [ 6, 6 ] )
                , ( 7, [ 7, 7 ] )
                , ( 8, [ 8, 8 ] )
                , ( 9, [ 9, 9 ] )
                , ( 10, [ 10, 10 ] )
                , ( 11, [ 11 ] )
                , ( 12, [ 12 ] )
                , ( 13, [ 13 ] )
                , ( 14, [ 14 ] )
                , ( 15, [ 15 ] )
                ]
    in
        describe "merge"
            [ test "merge of empty dicts returns empty dict"
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
