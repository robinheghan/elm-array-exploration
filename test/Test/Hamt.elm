module Test.Hamt exposing (tests)

import Test exposing (..)
import Expect
import CollectionsNg.Hamt exposing (..)


tests : Test
tests =
    describe "Hamt"
        [ collisions
        , foldl'
        , equality
        ]


hash : Int
hash =
    0x357DEDBF


collisionHamt : Tree String String
collisionHamt =
    empty
        |> set 65 "Key1" "Val1"
        |> set 65 "Key2" "Val2"
        |> set 65 "Key2" "Val3"
        |> set 64 "Key4" "Val4"


collisions : Test
collisions =
    describe "Collisions"
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


foldl' : Test
foldl' =
    describe "foldl"
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


equality : Test
equality =
    describe "Equality"
        [ test "should be equal"
            <| \() -> Expect.equal equalityHamt equalityHamt'
        ]
