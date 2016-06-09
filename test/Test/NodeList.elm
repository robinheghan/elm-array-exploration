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
        , foldlCheck
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
        |> set 65 "Key1" "Val1"
        |> set 65 "Key2" "Val2"
        |> set 65 "Key2" "Val3"
        |> set 64 "Key4" "Val4"


collisionCheck : Test
collisionCheck =
    suite "CollisionCheck Tests"
        [ test "first key"
            <| assertEqual (Just "Val1")
            <| get 65 "Key1" collisionNodeList
        , test "second key"
            <| assertEqual (Just "Val3")
            <| get 65 "Key2" collisionNodeList
        , test "third key"
            <| assertEqual (Just "Val4")
            <| get 64 "Key4" collisionNodeList
        ]


foldlNodeList : NodeList String String
foldlNodeList =
    collisionNodeList
        |> set 64 "Key5" "Val5"
        |> set 31 "Key6" "Val6"


foldlCheck : Test
foldlCheck =
    suite "Foldl tests"
        [ test "can extract all values"
            <| assertEqual
                [ ( "Key6", "Val6" )
                , ( "Key1", "Val1" )
                , ( "Key2", "Val3" )
                , ( "Key4", "Val4" )
                , ( "Key5", "Val5" )
                ]
            <| foldl (\k v acc -> ( k, v ) :: acc) [] foldlNodeList
        ]
