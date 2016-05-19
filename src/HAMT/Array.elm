module HAMT.Array exposing (..)

import HAMT.NodeList as NodeList exposing (NodeList)


type alias HArray a =
    { length : Int
    , nodes : NodeList a
    }


empty : HArray a
empty =
    HArray 0 NodeList.empty


isEmpty : HArray a -> Bool
isEmpty arr =
    length arr == 0


length : HArray a -> Int
length arr =
    arr.length


fromList : List a -> HArray a
fromList ls =
    List.foldl fromList' empty ls


fromList' : a -> HArray a -> HArray a
fromList' n arr =
    push n arr


toList : HArray a -> List a
toList arr =
    toList' (arr.length - 1) [] arr


toList' : Int -> List a -> HArray a -> List a
toList' idx acc arr =
    if idx == -1 then
        acc
    else
        let
            val =
                NodeList.get idx 0 arr.nodes
        in
            case val of
                NodeList.Element _ x ->
                    toList' (idx - 1) (x :: acc) arr

                _ ->
                    Debug.crash "Not a proper array"


push : a -> HArray a -> HArray a
push a arr =
    { length = arr.length + 1
    , nodes = NodeList.set arr.length 0 a arr.nodes
    }


pop : HArray a -> HArray a
pop arr =
    if arr.length == 0 then
        arr
    else
        { length = arr.length - 1
        , nodes = NodeList.remove (arr.length - 1) arr.nodes
        }


get : Int -> HArray a -> Maybe a
get idx arr =
    if idx >= arr.length || idx < 0 then
        Nothing
    else
        let
            val =
                NodeList.get idx 0 arr.nodes
        in
            case val of
                NodeList.Element _ x ->
                    Just x

                _ ->
                    Nothing


set : Int -> a -> HArray a -> HArray a
set idx val arr =
    if idx >= arr.length || idx < 0 then
        arr
    else
        { arr | nodes = NodeList.set idx 0 val arr.nodes }
