module HAMT.Set
    exposing
        ( HSet
        , empty
        , singleton
        , insert
        , remove
        , isEmpty
        , member
        , size
        , foldl
        , foldr
        , map
        , filter
        , partition
        , union
        , intersect
        , diff
        , toList
        , fromList
        )

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.
Insert, remove, and query operations all take *O(log n)* time. Set equality with
`(==)` is unreliable and should not be used.
# Sets
@docs Set
# Build
@docs empty, singleton, insert, remove
# Query
@docs isEmpty, member, size
# Combine
@docs union, intersect, diff
# Lists
@docs toList, fromList
# Transform
@docs map, foldl, foldr, filter, partition
-}

import HAMT.Dict as Dict


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type HSet t
    = Dict.HDict t ()


{-| Create an empty set.
-}
empty : HSet a
empty =
    Dict.empty


{-| Create a set with one value.
-}
singleton : comparable -> HSet comparable
singleton k =
    Dict.singleton k ()


{-| Insert a value into a set.
-}
insert : comparable -> HSet comparable -> HSet comparable
insert k d =
    Dict.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : comparable -> HSet comparable -> HSet comparable
remove k d =
    Dict.remove k d


{-| Determine if a set is empty.
-}
isEmpty : HSet a -> Bool
isEmpty d =
    Dict.isEmpty d


{-| Determine if a value is in a set.
-}
member : comparable -> HSet comparable -> Bool
member k d =
    Dict.member k d


{-| Determine the number of elements in a set.
-}
size : HSet a -> Int
size d =
    Dict.size d


{-| Get the union of two sets. Keep all values.
-}
union : HSet comparable -> HSet comparable -> HSet comparable
union d1 d2 =
    Dict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : HSet comparable -> HSet comparable -> HSet comparable
intersect d1 d2 =
    Dict.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : HSet comparable -> HSet comparable -> HSet comparable
diff d1 d2 =
    Dict.diff d1 d2


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : HSet comparable -> List comparable
toList d =
    Dict.keys d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List comparable -> HSet comparable
fromList xs =
    List.foldl insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (comparable -> b -> b) -> b -> HSet comparable -> b
foldl f b d =
    Dict.foldl (\k _ b -> f k b) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (comparable -> b -> b) -> b -> HSet comparable -> b
foldr f b d =
    Dict.foldr (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (comparable -> comparable') -> HSet comparable -> HSet comparable'
map f s =
    fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (comparable -> Bool) -> HSet comparable -> HSet comparable
filter p d =
    Dict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (comparable -> Bool) -> HSet comparable -> ( HSet comparable, HSet comparable )
partition p d =
    Dict.partition (\k _ -> p k) d
