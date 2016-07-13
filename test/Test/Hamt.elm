module Test.Hamt exposing (tests)

import Test exposing (..)
import Expect
import CollectionsNg.Hamt exposing (..)


hash : Int
hash =
    0x357DEDBF


tests : Test
tests =
    describe "Hamt tests"
        [ hashPath
        , bitCount
        , collisionCheck
        , foldlCheck
        , equalityCheck
        ]


hashPath : Test
hashPath =
    describe "HashPath Tests"
        [ test "first level"
            <| \() ->
                hashPositionWithShift 0 hash
                    |> Expect.equal 31
        , test "second level"
            <| \() ->
                hashPositionWithShift 5 hash
                    |> Expect.equal 13
        , test "third level"
            <| \() ->
                hashPositionWithShift 10 hash
                    |> Expect.equal 27
        , test "fourth level"
            <| \() ->
                hashPositionWithShift 15 hash
                    |> Expect.equal 27
        , test "fifth level"
            <| \() ->
                hashPositionWithShift 20 hash
                    |> Expect.equal 23
        , test "sixth level"
            <| \() ->
                hashPositionWithShift 25 hash
                    |> Expect.equal 26
        ]


bitCount : Test
bitCount =
    describe "BitCount Tests"
        [ test "0"
            <| \() ->
                countBits 0
                    |> Expect.equal 0
        , test "1"
            <| \() ->
                countBits 0x00080000
                    |> Expect.equal 1
        , test "32"
            <| \() ->
                countBits 0xFFFFFFFF
                    |> Expect.equal 32
        , test "16"
            <| \() ->
                countBits 0xFFFF
                    |> Expect.equal 16
        , test "8"
            <| \() ->
                countBits 0xFF
                    |> Expect.equal 8
        , test "4"
            <| \() ->
                countBits 0x0F
                    |> Expect.equal 4
        , test "7"
            <| \() ->
                countBits 0x0202A822
                    |> Expect.equal 7
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
    describe "CollisionCheck Tests"
        [ test "first key"
            <| \() ->
                get 65 "Key1" collisionHamt
                    |> Expect.equal (Just "Val1")
        , test "second key"
            <| \() ->
                get 65 "Key2" collisionHamt
                    |> Expect.equal (Just "Val3")
        , test "third key"
            <| \() ->
                get 64 "Key4" collisionHamt
                    |> Expect.equal (Just "Val4")
        ]


foldlHamt : Tree String String
foldlHamt =
    collisionHamt
        |> set 64 "Key5" "Val5"
        |> set 31 "Key6" "Val6"


foldlCheck : Test
foldlCheck =
    describe "Foldl tests"
        [ test "can extract all values"
            <| \() ->
                foldl (\k v acc -> ( k, v ) :: acc) [] foldlHamt
                    |> Expect.equal
                        [ ( "Key6", "Val6" )
                        , ( "Key2", "Val3" )
                        , ( "Key1", "Val1" )
                        , ( "Key5", "Val5" )
                        , ( "Key4", "Val4" )
                        ]
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
    describe "Equality"
        [ test "should be equal"
            <| \() -> Expect.equal equalityHamt equalityHamt'
        ]
