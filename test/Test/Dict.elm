module Test.Dict exposing (tests)

import HAMT.Dict exposing (..)
import ElmTest exposing (..)


tests : Test
tests =
    suite "Dict tests"
        [ emptyTests
        , sizeTests
        , getSetTests
        ]


simpleDict : HDict String String
simpleDict =
    fromList
        [ ( "Key1", "Val1" )
        , ( "Key2", "Val2" )
        , ( "Key3", "Val3" )
        ]


emptyTests : Test
emptyTests =
    suite "Empty"
        [ test "empty has 0 size" <| assertEqual 0 <| size empty
        , test "0 size is empty" <| assert <| isEmpty empty
        ]


sizeTests : Test
sizeTests =
    suite "Length"
        [ test "simple count" <| assertEqual 3 <| size simpleDict
        , test "insert increases size" <| assertEqual 4 <| size (insert "Key4" "Val4" simpleDict)
        , test "remove decreases size" <| assertEqual 2 <| size (remove "Key2" simpleDict)
        , test "singleton has 1 size" <| assertEqual 1 <| size (singleton "Key1" "Val1")
        ]


getSetTests : Test
getSetTests =
    suite "Get set"
        [ test "get from singleton"
            <| assertEqual (Just "Val1")
            <| get "Key1" (singleton "Key1" "Val1")
        , test "simple get"
            <| assertEqual (Just "Val2")
            <| get "Key2" simpleDict
        , test "nonexistant get"
            <| assertEqual Nothing
            <| get "Key4" simpleDict
        , test "simple set"
            <| assertEqual (Just "Val4")
            <| get "Key4" (insert "Key4" "Val4" simpleDict)
        , test "simple remove"
            <| assertEqual Nothing
            <| get "Key2" (remove "Key2" simpleDict)
        , test "simple update"
            <| assertEqual (Just "Val3Val")
            <| get "Key3" (update (always (Just "Val3Val")) "Key3" simpleDict)
        , test "upsert"
            <| assertEqual (Just "Val5")
            <| get "Key5" (update (always (Just "Val5")) "Key5" simpleDict)
        , test "remove by update"
            <| assertEqual Nothing
            <| get "Key2" (update (always Nothing) "Key2" simpleDict)
        , test "member exists"
            <| assert
            <| member "Key2" simpleDict
        , test "member nonexist"
            <| assertEqual False
            <| member "Key4" simpleDict
        ]
