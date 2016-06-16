module CollectionsNg.Array
    exposing
        ( Array
        , empty
        , isEmpty
        , length
        , initialize
        , repeat
        , fromList
        , toList
        , toIndexedList
        , push
        , pop
        , get
        , set
        , foldr
        , foldl
        , append
        , filter
        , map
        , indexedMap
        , slice
        )

{-| Fast immutable arrays. The elements in an array must have the
same type.

# Arrays
@docs Array

# Creation
@docs empty, repeat, initialize, fromList

# Query
@docs isEmpty, length, get

# Manipulate
@docs set, push, pop, append, slice

# Lists
@docs toList, toIndexedList

# Transform
@docs map, indexedMap, filter, foldl, foldr
-}

import CollectionsNg.Hamt as Hamt exposing (Tree)


{-| Representation of fast immutable arrays. You can create arrays of integers
(`Array Int`) or strings (`Array String`) or any other type of value you can
dream up.
-}
type alias Array a =
    { length : Int
    , nodes : Tree Int a
    }


{-| Return an empty array.

    length empty == 0
-}
empty : Array a
empty =
    Array 0 Hamt.empty


{-| Determine if an array is empty.

    isEmpty empty == True
-}
isEmpty : Array a -> Bool
isEmpty arr =
    arr.length == 0


{-| Return the length of an array.

    length (fromList [1,2,3]) == 3
-}
length : Array a -> Int
length arr =
    arr.length


{-| Initialize an array. `initialize n f` creates an array of length `n` with
the element at index `i` initialized to the result of `(f i)`.

    initialize 4 identity    == fromList [0,1,2,3]
    initialize 4 (\n -> n*n) == fromList [0,1,4,9]
    initialize 4 (always 0)  == fromList [0,0,0,0]
-}
initialize : Int -> (Int -> a) -> Array a
initialize stop f =
    initialize' stop 0 f empty


initialize' : Int -> Int -> (Int -> a) -> Array a -> Array a
initialize' stop idx f acc =
    if stop <= idx then
        acc
    else
        initialize' stop (idx + 1) f <| push (f idx) acc


{-| Creates an array with a given length, filled with a default element.

    repeat 5 0     == fromList [0,0,0,0,0]
    repeat 3 "cat" == fromList ["cat","cat","cat"]

Notice that `repeat 3 x` is the same as `initialize 3 (always x)`.
-}
repeat : Int -> a -> Array a
repeat n e =
    initialize n (always e)


{-| Create an array from a list.
-}
fromList : List a -> Array a
fromList ls =
    List.foldl push empty ls


{-| Create a list of elements from an array.

    toList (fromList [3,5,8]) == [3,5,8]
-}
toList : Array a -> List a
toList arr =
    foldr (\acc n -> n :: acc) [] arr


{-| Create an indexed list from an array. Each element of the array will be
paired with its index.

    toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
-}
toIndexedList : Array a -> List ( Int, a )
toIndexedList arr =
    List.map2 (,) [0..(arr.length - 1)] (toList arr)


{-| Push an element to the end of an array.

    push 3 (fromList [1,2]) == fromList [1,2,3]
-}
push : a -> Array a -> Array a
push a arr =
    { length = arr.length + 1
    , nodes = Hamt.set arr.length arr.length a arr.nodes
    }


{-| Remove the last element from the array.

    pop (fromList [1,2]) == fromList [1]
-}
pop : Array a -> Array a
pop arr =
    if isEmpty arr then
        arr
    else
        let
            lastIndex =
                arr.length - 1
        in
            { length = lastIndex
            , nodes = Hamt.remove lastIndex lastIndex arr.nodes
            }


{-| Return Just the element at the index or Nothing if the index is out of range.

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Nothing
-}
get : Int -> Array a -> Maybe a
get idx arr =
    if idx >= arr.length || idx < 0 then
        Nothing
    else
        Hamt.get idx idx arr.nodes


{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-}
set : Int -> a -> Array a -> Array a
set idx val arr =
    if idx >= arr.length || idx < 0 then
        arr
    else
        { arr | nodes = Hamt.set idx idx val arr.nodes }


{-| Reduce an array from the right. Read `foldr` as fold from the right.

    foldr (+) 0 (repeat 3 5) == 15
-}
foldr : (b -> a -> b) -> b -> Array a -> b
foldr folder init arr =
    foldr' folder init (arr.length - 1) arr


foldr' : (b -> a -> b) -> b -> Int -> Array a -> b
foldr' folder acc idx arr =
    if idx == -1 then
        acc
    else
        case Hamt.get idx idx arr.nodes of
            Just x ->
                foldr' folder (folder acc x) (idx - 1) arr

            Nothing ->
                Debug.crash "This is a bug. Please report this."


{-| Reduce an array from the left. Read `foldl` as fold from the left.

    foldl (::) [] (fromList [1,2,3]) == [3,2,1]
-}
foldl : (a -> b -> b) -> b -> Array a -> b
foldl folder init arr =
    foldl' folder init 0 arr


foldl' : (a -> b -> b) -> b -> Int -> Array a -> b
foldl' folder acc idx arr =
    if idx == arr.length then
        acc
    else
        case Hamt.get idx idx arr.nodes of
            Just x ->
                foldl' folder (folder x acc) (idx + 1) arr

            Nothing ->
                Debug.crash "This is a bug. Please report this."


{-| Append two arrays to a new one.

    append (repeat 2 42) (repeat 3 81) == fromList [42,42,81,81,81]
-}
append : Array a -> Array a -> Array a
append a b =
    foldl push a b


{-| Keep only elements that satisfy the predicate:

    filter isEven (fromList [1..6]) == (fromList [2,4,6])
-}
filter : (a -> Bool) -> Array a -> Array a
filter pred arr =
    let
        update n acc =
            if pred n then
                push n acc
            else
                acc
    in
        foldl update empty arr


{-| Apply a function on every element in an array.

    map sqrt (fromList [1,4,9]) == fromList [1,2,3]
-}
map : (a -> b) -> Array a -> Array b
map mapper arr =
    foldl (\n acc -> push (mapper n) acc) empty arr


{-| Apply a function on every element with its index as first argument.

    indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
-}
indexedMap : (Int -> a -> b) -> Array a -> Array b
indexedMap mapper arr =
    indexedMap' mapper empty 0 arr


indexedMap' : (Int -> a -> b) -> Array b -> Int -> Array a -> Array b
indexedMap' mapper acc idx arr =
    if idx == arr.length then
        acc
    else
        case Hamt.get idx idx arr.nodes of
            Just x ->
                indexedMap' mapper (push (mapper idx x) acc) (idx + 1) arr

            Nothing ->
                Debug.crash "This is a bug. Please report this."


{-| Get a sub-section of an array: `(slice start end array)`. The `start` is a
zero-based index where we will start our slice. The `end` is a zero-based index
that indicates the end of the slice. The slice extracts up to but not including
`end`.

    slice  0  3 (fromList [0,1,2,3,4]) == fromList [0,1,2]
    slice  1  4 (fromList [0,1,2,3,4]) == fromList [1,2,3]

Both the `start` and `end` indexes can be negative, indicating an offset from
the end of the array.

    slice  1 -1 (fromList [0,1,2,3,4]) == fromList [1,2,3]
    slice -2  5 (fromList [0,1,2,3,4]) == fromList [3,4]

This makes it pretty easy to `pop` the last element off of an array: `slice 0 -1 array`
-}
slice : Int -> Int -> Array a -> Array a
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


slice' : Int -> Int -> Array a -> Array a -> Array a
slice' from to acc arr =
    if from == to then
        acc
    else
        case Hamt.get from from arr.nodes of
            Just x ->
                slice' (from + 1) to (push x acc) arr

            Nothing ->
                Debug.crash "This is a bug. Please report this."


translateIndex : Int -> Array a -> Int
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
