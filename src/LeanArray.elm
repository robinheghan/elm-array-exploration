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
    List.foldl fromList' empty ls


fromList' : a -> LeanArray a -> LeanArray a
fromList' n arr =
    push n arr


toList : LeanArray a -> List a
toList arr =
    toList' (arr.length - 1) [] arr


toList' : Int -> List a -> LeanArray a -> List a
toList' idx acc arr =
    if idx == -1 then
        acc
    else
        let
            val =
                HAMT.get idx arr
        in
            case val of
                HAMT.Element _ x ->
                    toList' (idx - 1) (x :: acc) arr

                _ ->
                    Debug.crash "Not a proper array"


push : a -> LeanArray a -> LeanArray a
push a arr =
    HAMT.set arr.length a arr


pop : LeanArray a -> LeanArray a
pop arr =
    HAMT.remove (arr.length - 1) arr


get : Int -> LeanArray a -> Maybe a
get idx arr =
    let
        val =
            HAMT.get idx arr
    in
        case val of
            HAMT.Element _ x ->
                Just x

            _ ->
                Nothing


insertAt : Int -> LeanArray a -> LeanArray a
insertAt idx arr =
    arr


removeAt : Int -> LeanArray a -> LeanArray a
removeAt idx arr =
    arr
