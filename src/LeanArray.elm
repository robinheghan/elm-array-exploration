module LeanArray exposing (..)

import HAMT exposing (HAMT)


type alias LeanArray a =
    HAMT a


empty : LeanArray a
empty =
    HAMT.empty


isEmpty : LeanArray a -> Bool
isEmpty arr =
    length arr == 0


length : LeanArray a -> Int
length arr =
    arr.length


fromList : List a -> LeanArray a
fromList ls =
    empty


toList : LeanArray a -> List a
toList arr =
    []


push : a -> LeanArray a -> LeanArray a
push a arr =
    arr


pop : LeanArray a -> LeanArray a
pop arr =
    arr


get : Int -> LeanArray a -> Maybe a
get idx arr =
    Nothing


insertAt : Int -> LeanArray a -> LeanArray a
insertAt idx arr =
    arr


removeAt : Int -> LeanArray a -> LeanArray a
removeAt idx arr =
    arr
