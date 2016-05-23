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
