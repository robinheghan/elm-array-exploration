module Test.Array exposing (tests)

import HAMT.Array exposing (..)
import ElmTest exposing (..)


tests : Test
tests =
    suite "Array tests"
        [ isEmpty'
        , length'
        , getSet
        , conversion
        , stack
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
        , test "large array" <| assertEqual 60 <| length (fromList [1..60])
        , test "push" <| assertEqual 3 <| length (push 3 (fromList [ 1, 2 ]))
        , test "pop" <| assertEqual 2 <| length (pop (fromList [ 1, 2, 3 ]))
        ]


getSet : Test
getSet =
    suite "Testing simple get and set functionality"
        [ test "can retrieve element" <| assertEqual (Just 2) <| get 1 (fromList [ 1, 2, 3 ])
        , test "can retrieve element in large array" <| assertEqual (Just 45) <| get 45 (fromList [0..60])
        , test "out of bounds retrieval returns nothing" <| assertEqual Nothing <| get 1 (fromList [ 1 ])
        , test "set replaces value" <| assertEqual [ 1, 5, 3 ] <| toList <| set 1 5 (fromList [ 1, 2, 3 ])
        , test "set out of bounds returns original array" <| assertEqual [ 1, 2, 3 ] <| toList <| set 3 5 (fromList [ 1, 2, 3 ])
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
        [ test "pop empty array returns empty array" <| assertEqual [] <| toList (pop empty)
        , test "pop removes last element" <| assertEqual [ 1, 2 ] <| toList (pop (fromList [ 1, 2, 3 ]))
        , test "push appends one element" <| assertEqual [ 1, 2, 3 ] <| toList (push 3 (fromList [ 1, 2 ]))
        ]
