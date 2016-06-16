module Test.Array exposing (tests)

import CollectionsNg.Array exposing (..)
import ElmTest exposing (..)


tests : Test
tests =
    suite "Array tests"
        [ init'
        , isEmpty'
        , length'
        , getSet
        , conversion
        , stack
        , transform
        ]


init' : Test
init' =
    suite "Initialization Tests"
        [ test "initialize" <| assertEqual [ 0, 1, 2, 3 ] <| toList (initialize 4 identity)
        , test "repeat" <| assertEqual [ 0, 0, 0 ] <| toList (repeat 3 0)
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
        , test "append" <| assertEqual 5 <| length (append (fromList [ 1, 2 ]) (fromList [ 3, 4, 5 ]))
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
        , test "indexed" <| assertEqual [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ) ] <| toIndexedList (fromList [ 1, 2, 3 ])
        ]


stack : Test
stack =
    suite "Stack tests"
        [ test "pop empty array returns empty array" <| assertEqual [] <| toList (pop empty)
        , test "pop removes last element" <| assertEqual [ 1, 2 ] <| toList (pop (fromList [ 1, 2, 3 ]))
        , test "push appends one element" <| assertEqual [ 1, 2, 3 ] <| toList (push 3 (fromList [ 1, 2 ]))
        ]


transform : Test
transform =
    suite "Transform tests"
        [ test "foldl" <| assertEqual [ 3, 2, 1 ] <| foldl (::) [] (fromList [ 1, 2, 3 ])
        , test "foldr" <| assertEqual [ 1, 2, 3 ] <| foldr (\acc n -> n :: acc) [] (fromList [ 1, 2, 3 ])
        , test "filter" <| assertEqual [ 2, 4, 6 ] <| toList (filter (\a -> a % 2 == 0) (fromList [1..6]))
        , test "map" <| assertEqual [ 2, 3, 4 ] <| toList (map (\a -> a + 1) (fromList [ 1, 2, 3 ]))
        , test "indexedMap" <| assertEqual [ 0, 5, 10 ] <| toList (indexedMap (*) (fromList [ 5, 5, 5 ]))
        , test "append" <| assertEqual [1..120] <| toList (append (fromList [1..60]) (fromList [61..120]))
        , test "slice" <| assertEqual [3..6] <| toList (slice 2 5 (fromList [1..8]))
        , test "negative slice" <| assertEqual [3..6] <| toList (slice -5 -2 (fromList [1..8]))
        , test "combined slice" <| assertEqual [3..6] <| toList (slice 2 -2 (fromList [1..8]))
        , test "impossible slice" <| assertEqual [] <| toList (slice 6 -2 (fromList [1..8]))
        ]
