module Test.NodeList exposing (tests)

import HAMT.NodeList exposing (..)
import HAMT.Hash as Hash
import ElmTest exposing (..)


hash : Int
hash =
    0x357DEDBF


tests : Test
tests =
    suite "NodeList tests"
        [ hashPath
        , hashing
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


hashFn : String -> Int
hashFn =
    Hash.string 63476


hashing : Test
hashing =
    suite "Hashing"
        [ test "int" <| assertEqual 1992578978 <| hashFn <| toString -102433675
        , test "float" <| assertEqual 335970363 <| hashFn <| toString 4.32
        , test "rec" <| assertEqual 3455049611 <| hashFn <| toString { name = "Robin", age = "27", male = True }
        , test "tuple" <| assertEqual 12752532 <| hashFn <| toString ( "Robin", 27, True )
        , test "ls" <| assertEqual 4202619459 <| hashFn <| toString [1..6]
        , test "bool" <| assertEqual 108766572 <| hashFn <| toString False
        ]
