module Test.LeanArray exposing (tests)

import LeanArray exposing (..)
import ElmTest exposing (..)


tests : Test
tests =
    suite "Array tests"
        [ isEmpty'
        , length'
        , conversion
        , stack
        , indexManipulation
        ]


isEmpty' : Test
isEmpty' =
    suite "isEmpty Tests"
        [ test "empty array" <| assert <| isEmpty empty
        , test "empty converted array" <| assert <| isEmpty (fromList [])
        , test "non-empty array" <| assert <| not (isEmpty (fromList [ 1 ]))
        ]


length' : Test
length' =
    suite "Length tests"
        [ test "empty array" <| assertEqual 0 <| length empty
        , test "array of one" <| assertEqual 1 <| length (fromList [ 1 ])
        , test "array of two" <| assertEqual 2 <| length (fromList [ 1, 2 ])
        , test "array of three" <| assertEqual 3 <| length (fromList [ 1, 2, 3 ])
        ]


conversion : Test
conversion =
    suite "Conversion tests"
        [ test "empty array" <| assertEqual [] <| toList (fromList [])
        , test "correct element" <| assertEqual [ 1, 2, 3 ] <| toList (fromList [ 1, 2, 3 ])
        ]


stack : Test
stack =
    suite "Stack tests"
        []


indexManipulation : Test
indexManipulation =
    suite "Index manipulation tests"
        []
