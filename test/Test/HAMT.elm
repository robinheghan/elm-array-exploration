module Test.HAMT exposing (tests)

import HAMT exposing (..)
import ElmTest exposing (..)


hash : Int
hash =
    0x357DEDBF


tests : Test
tests =
    suite "Hamt tests"
        [ hashPath
        ]


hashPath : Test
hashPath =
    suite "HashPath Tests"
        [ test "first level" <| assertEqual 31 <| hashPositionAtDepth 0 hash
        , test "second level" <| assertEqual 13 <| hashPositionAtDepth 1 hash
        , test "third level" <| assertEqual 27 <| hashPositionAtDepth 2 hash
        , test "fourth level" <| assertEqual 27 <| hashPositionAtDepth 3 hash
        , test "fifth level" <| assertEqual 23 <| hashPositionAtDepth 4 hash
        , test "sixth level" <| assertEqual 26 <| hashPositionAtDepth 5 hash
        ]
