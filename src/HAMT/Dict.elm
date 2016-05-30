module HAMT.Dict exposing (..)

import HAMT.NodeList as NodeList exposing (NodeList)
import Hash


type alias HDict comparable v =
    { size : Int
    , nodes : NodeList comparable v
    }


hashFn : a -> Int
hashFn obj =
    Hash.string 19456 <| toString obj


empty : HDict comparable v
empty =
    HDict 0 NodeList.empty


isEmpty : HDict comparable v -> Bool
isEmpty dict =
    dict.size == 0


size : HDict comparable v -> Int
size dict =
    dict.size


get : comparable -> HDict comparable v -> Maybe v
get key dict =
    NodeList.get (hashFn key) key dict.nodes


member : comparable -> HDict comparable v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


set : comparable -> v -> HDict comparable v -> HDict comparable v
set key value dict =
    { size = dict.size + 1
    , nodes = NodeList.set (hashFn key) key value dict.nodes
    }


remove : comparable -> HDict comparable v -> HDict comparable v
remove key dict =
    { size = dict.size - 1
    , nodes = NodeList.remove (hashFn key) key dict.nodes
    }


fromList : List ( comparable, v ) -> HDict comparable v
fromList list =
    List.foldl (\( key, value ) acc -> set key value acc) empty list


toList : HDict comparable v -> List ( comparable, v )
toList dict =
    []
