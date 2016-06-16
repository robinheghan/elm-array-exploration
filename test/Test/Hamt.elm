module Test.Hamt exposing (tests)

import CollectionsNg.Hamt exposing (..)
import ElmTest exposing (..)


hash : Int
hash =
    0x357DEDBF


tests : Test
tests =
    suite "Hamt tests"
        [ hashPath
        , bitCount
        , collisionCheck
        , foldlCheck
        , equalityCheck
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


bitCount : Test
bitCount =
    suite "BitCount Tests"
        [ test "0" <| assertEqual 0 <| countBits 0
        , test "1" <| assertEqual 1 <| countBits 0x00080000
        , test "32" <| assertEqual 32 <| countBits 0xFFFFFFFF
        , test "16" <| assertEqual 16 <| countBits 0xFFFF
        , test "8" <| assertEqual 8 <| countBits 0xFF
        , test "4" <| assertEqual 4 <| countBits 0x0F
        , test "7" <| assertEqual 7 <| countBits 0x0202A822
        ]


collisionHamt : Tree String String
collisionHamt =
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
            <| get 65 "Key1" collisionHamt
        , test "second key"
            <| assertEqual (Just "Val3")
            <| get 65 "Key2" collisionHamt
        , test "third key"
            <| assertEqual (Just "Val4")
            <| get 64 "Key4" collisionHamt
        ]


foldlHamt : Tree String String
foldlHamt =
    collisionHamt
        |> set 64 "Key5" "Val5"
        |> set 31 "Key6" "Val6"


foldlCheck : Test
foldlCheck =
    suite "Foldl tests"
        [ test "can extract all values"
            <| assertEqual
                [ ( "Key6", "Val6" )
                , ( "Key2", "Val3" )
                , ( "Key1", "Val1" )
                , ( "Key5", "Val5" )
                , ( "Key4", "Val4" )
                ]
            <| foldl (\k v acc -> ( k, v ) :: acc) [] foldlHamt
        ]


equalityHamt : Tree String Int
equalityHamt =
    empty
        |> set 5 "SimpleElement" 0
        |> set 1 "Sub" 4
        |> set 96 "Collision1" 1
        |> set 96 "Collision2" 2
        |> set 96 "Collision3" 3
        |> set 34 "ToCollide1" 4
        |> set 34 "ToCollide2" 5


equalityHamt' : Tree String Int
equalityHamt' =
    equalityHamt
        |> remove 5 "SimpleElement"
        |> set 5 "SimpleElement" 0
        |> set 257 "Sub2" 4
        |> remove 257 "Sub2"
        |> remove 96 "Collision2"
        |> set 96 "Collision2" 2
        |> remove 34 "ToCollide1"
        |> set 34 "ToCollide1" 4


equalityCheck : Test
equalityCheck =
    suite "Equality"
        [ test "should be equal" <| assertEqual equalityHamt equalityHamt' ]
