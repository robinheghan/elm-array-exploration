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
        , test "insert increases size" <| assertEqual 4 <| size (set "Key4" "Val4" simpleDict)
        , test "remove decreases size" <| assertEqual 2 <| size (remove "Key2" simpleDict)
        ]


getSetTests : Test
getSetTests =
    suite "Get set"
        [ test "simple get"
            <| assertEqual (Just "Val2")
            <| get "Key2" simpleDict
        , test "nonexistant get"
            <| assertEqual Nothing
            <| get "Key4" simpleDict
        , test "simple set"
            <| assertEqual (Just "Val4")
            <| get "Key4" (set "Key4" "Val4" simpleDict)
        , test "simple remove"
            <| assertEqual Nothing
            <| get "Key2" (remove "Key2" simpleDict)
        , test "member exists"
            <| assert
            <| member "Key2" simpleDict
        , test "member nonexist"
            <| assertEqual False
            <| member "Key4" simpleDict
        ]
