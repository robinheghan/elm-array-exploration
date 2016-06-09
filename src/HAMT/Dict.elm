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
    foldl (\k v acc -> ( k, v ) :: acc) [] dict


keys : HDict comparable v -> List comparable
keys dict =
    foldl (\k _ acc -> k :: acc) [] dict


values : HDict comparable v -> List v
values dict =
    foldl (\_ v acc -> v :: acc) [] dict



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (comparable -> a -> b) -> HDict comparable a -> HDict comparable b
map f dict =
    NodeList.foldl (\key val acc -> insert key (f key val) acc) empty dict.nodes


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (comparable -> v -> b -> b) -> b -> HDict comparable v -> b
foldl f acc dict =
    NodeList.foldl f acc dict.nodes


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (comparable -> v -> b -> b) -> b -> HDict comparable v -> b
foldr f acc t =
    NodeList.foldl f acc t.nodes


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> HDict comparable v -> HDict comparable v
filter predicate dictionary =
    let
        add key value dict =
            if predicate key value then
                insert key value dict
            else
                dict
    in
        foldl add empty dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (comparable -> v -> Bool) -> HDict comparable v -> ( HDict comparable v, HDict comparable v )
partition predicate dict =
    let
        add key value ( t1, t2 ) =
            if predicate key value then
                ( insert key value t1, t2 )
            else
                ( t1, insert key value t2 )
    in
        foldl add ( empty, empty ) dict



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : HDict comparable v -> HDict comparable v -> HDict comparable v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second HDictionary.
Preference is given to values in the first HDictionary.
-}
intersect : HDict comparable v -> HDict comparable v -> HDict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second HDictionary.
-}
diff : HDict comparable v -> HDict comparable v -> HDict comparable v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:
  1. Only in the left dictionary.
  2. In both dictionaries.
  3. Only in the right dictionary.
You then traverse all the keys from lowest to highest, building up whatever
you want.
-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> HDict comparable a
    -> HDict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        ( rest, leftStep lKey lValue result )
                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )
                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
        List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers
