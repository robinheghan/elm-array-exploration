module HAMT.Array exposing (..)

import HAMT.NodeList as NodeList exposing (NodeList)


type alias HArray a =
    { length : Int
    , nodes : NodeList Int a
    }


empty : HArray a
empty =
    HArray 0 NodeList.empty


isEmpty : HArray a -> Bool
isEmpty arr =
    arr.length == 0


length : HArray a -> Int
length arr =
    arr.length


fromList : List a -> HArray a
fromList ls =
    List.foldl push empty ls


toList : HArray a -> List a
toList arr =
    foldr (\acc n -> n :: acc) [] arr


toIndexedList : HArray a -> List ( Int, a )
toIndexedList arr =
    List.map2 (,) [0..(arr.length - 1)] (toList arr)


push : a -> HArray a -> HArray a
push a arr =
    { length = arr.length + 1
    , nodes = NodeList.set 0 arr.length arr.length a arr.nodes
    }


pop : HArray a -> HArray a
pop arr =
    if isEmpty arr then
        arr
    else
        let
            lastIndex =
                arr.length - 1
        in
            { length = lastIndex
            , nodes = NodeList.remove lastIndex arr.nodes
            }


get : Int -> HArray a -> Maybe a
get idx arr =
    if idx >= arr.length || idx < 0 then
        Nothing
    else
        case NodeList.get 0 idx idx arr.nodes of
            NodeList.Element _ _ x ->
                Just x

            _ ->
                Nothing


set : Int -> a -> HArray a -> HArray a
set idx val arr =
    if idx >= arr.length || idx < 0 then
        arr
    else
        { arr | nodes = NodeList.set 0 idx idx val arr.nodes }


foldr : (b -> a -> b) -> b -> HArray a -> b
foldr folder init arr =
    foldr' folder init (arr.length - 1) arr


foldr' : (b -> a -> b) -> b -> Int -> HArray a -> b
foldr' folder acc idx arr =
    if idx == -1 then
        acc
    else
        case NodeList.get 0 idx idx arr.nodes of
            NodeList.Element _ _ x ->
                foldr' folder (folder acc x) (idx - 1) arr

            _ ->
                Debug.crash "This is a bug. Please report this."


foldl : (a -> b -> b) -> b -> HArray a -> b
foldl folder init arr =
    foldl' folder init 0 arr


foldl' : (a -> b -> b) -> b -> Int -> HArray a -> b
foldl' folder acc idx arr =
    if idx == arr.length then
        acc
    else
        case NodeList.get 0 idx idx arr.nodes of
            NodeList.Element _ _ x ->
                foldl' folder (folder x acc) (idx + 1) arr

            _ ->
                Debug.crash "This is a bug. Please report this."


append : HArray a -> HArray a -> HArray a
append a b =
    foldl push a b


filter : (a -> Bool) -> HArray a -> HArray a
filter pred arr =
    let
        update n acc =
            if pred n then
                push n acc
            else
                acc
    in
        foldl update empty arr


map : (a -> b) -> HArray a -> HArray b
map mapper arr =
    foldl (\n acc -> push (mapper n) acc) empty arr


indexedMap : (Int -> a -> b) -> HArray a -> HArray b
indexedMap mapper arr =
    indexedMap' mapper empty 0 arr


indexedMap' : (Int -> a -> b) -> HArray b -> Int -> HArray a -> HArray b
indexedMap' mapper acc idx arr =
    if idx == arr.length then
        acc
    else
        case NodeList.get 0 idx idx arr.nodes of
            NodeList.Element _ _ x ->
                indexedMap' mapper (push (mapper idx x) acc) (idx + 1) arr

            _ ->
                Debug.crash "This is a bug. Please report this."


slice : Int -> Int -> HArray a -> HArray a
slice from to arr =
    let
        correctFrom =
            translateIndex from arr

        correctTo =
            translateIndex to arr
    in
        if isEmpty arr || correctFrom > correctTo then
            empty
        else
            slice' correctFrom (correctTo + 1) empty arr


slice' : Int -> Int -> HArray a -> HArray a -> HArray a
slice' from to acc arr =
    if from == to then
        acc
    else
        case NodeList.get 0 from from arr.nodes of
            NodeList.Element _ _ x ->
                slice' (from + 1) to (push x acc) arr

            _ ->
                Debug.crash "This is a bug. Please report this."


translateIndex : Int -> HArray a -> Int
translateIndex idx arr =
    let
        posIndex =
            if idx < 0 then
                arr.length - 1 + idx
            else
                idx
    in
        if posIndex < 0 then
            0
        else if posIndex >= arr.length then
            arr.length - 1
        else
            posIndex
