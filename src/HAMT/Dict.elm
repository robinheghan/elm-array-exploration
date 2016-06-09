module HAMT.Dict exposing (..)

import HAMT.NodeList as NodeList exposing (NodeList)
import Murmur3


type alias HDict comparable v =
    { size : Int
    , nodes : NodeList comparable v
    }


hashFn : a -> Int
hashFn obj =
    Murmur3.hashString 19456 <| toString obj


empty : HDict comparable v
empty =
    HDict 0 NodeList.empty


singleton : comparable -> v -> HDict comparable v
singleton key val =
    insert key val empty


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


insert : comparable -> v -> HDict comparable v -> HDict comparable v
insert key value dict =
    { size = dict.size + 1
    , nodes = NodeList.set (hashFn key) key value dict.nodes
    }


update : (Maybe v -> Maybe v) -> comparable -> HDict comparable v -> HDict comparable v
update fn key dict =
    case fn <| get key dict of
        Just val ->
            insert key val dict

        Nothing ->
            remove key dict


remove : comparable -> HDict comparable v -> HDict comparable v
remove key dict =
    { size = dict.size - 1
    , nodes = NodeList.remove (hashFn key) key dict.nodes
    }


fromList : List ( comparable, v ) -> HDict comparable v
fromList list =
    List.foldl (\( key, value ) acc -> insert key value acc) empty list


toList : HDict comparable v -> List ( comparable, v )
toList dict =
    []
