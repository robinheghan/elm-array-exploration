module Test.NodeList exposing (tests)

import HAMT.NodeList exposing (..)
import ElmTest exposing (..)


hash : Int
hash =
    0x357DEDBF


tests : Test
tests =
    suite "NodeList tests"
        [ hashPath
        , collisionCheck
        ]


hashPath : Test
hashPath =
    suite "HashPath Tests"
        [ test "first level" <| assertEqual 31 <| hashPositionWithShift 0 hash
        , test "second level" <| assertEqual 13 <| hashPositionWithShift 5 hash
        , test "third level" <| assertEqual 27 <| hashPositionWithShift 10 hash
        , test "fourth level" <| assertEqual 27 <| hashPositionWithShift 15 hash
        , test "fifth level" <| assertEqual 23 <| hashPositionWithShift 20 hash
        , test "sixth level" <| assertEqual 26 <| hashPositionWithShift 25 hash
        ]


collisionNodeList : NodeList String String
collisionNodeList =
    empty
        |> set 0 65 "Key1" "Val1"
        |> set 0 65 "Key2" "Val2"
        |> set 0 65 "Key2" "Val3"
        |> set 0 64 "Key4" "Val4"


collisionCheck : Test
collisionCheck =
    suite "CollisionCheck Tests"
        [ test "first key"
            <| assertEqual (Just "Val1")
            <| get 0 65 "Key1" collisionNodeList
        , test "second key"
            <| assertEqual (Just "Val3")
            <| get 0 65 "Key2" collisionNodeList
        , test "third key"
            <| assertEqual (Just "Val4")
            <| get 0 64 "Key4" collisionNodeList
        ]
