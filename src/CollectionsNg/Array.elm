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
@docs set, push, append, slice

# Lists
@docs toList, toIndexedList

# Transform
@docs map, indexedMap, filter, foldl, foldr
-}

import Bitwise
import CollectionsNg.Hamt exposing (hashPositionWithShift)
import Array as CoreArray


{-| Representation of fast immutable arrays. You can create arrays of integers
(`Array Int`) or strings (`Array String`) or any other type of value you can
dream up.
-}
type alias Array a =
    { length : Int
    , startShift : Int
    , tree : Tree a
    , tail : Tree a
    }


type alias Tree a =
    CoreArray.Array (Node a)


type Node a
    = Value a
    | SubTree (Tree a)


crashMsg : String
crashMsg =
    "This is a bug. Please report this."


{-| Return an empty array.

    length empty == 0
-}
empty : Array a
empty =
    Array 0 5 CoreArray.empty CoreArray.empty


{-| Determine if an array is empty.

    isEmpty empty == True
-}
isEmpty : Array a -> Bool
isEmpty arr =
    length arr == 0


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
    let
        initialize' idx acc =
            if stop <= idx then
                acc
            else
                initialize' (idx + 1) (push (f idx) acc)
    in
        initialize' 0 empty


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
    foldr (\n acc -> n :: acc) [] arr


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
    let
        newLen =
            arr.length + 1

        newShift =
            calcStartShift newLen

        newTail =
            CoreArray.push (Value a) arr.tail

        tailLen =
            CoreArray.length newTail

        newTree =
            if tailLen == 32 then
                tailPush arr.startShift arr.length newTail arr.tree
            else
                arr.tree
    in
        { length = newLen
        , startShift = newShift
        , tree =
            if newShift > arr.startShift then
                CoreArray.push (SubTree newTree) CoreArray.empty
            else
                newTree
        , tail =
            if tailLen == 32 then
                CoreArray.empty
            else
                newTail
        }


tailPush : Int -> Int -> Tree a -> Tree a -> Tree a
tailPush shift idx tail tree =
    let
        pos =
            hashPositionWithShift shift idx
    in
        case CoreArray.get pos tree of
            Just x ->
                case x of
                    SubTree subTree ->
                        let
                            newSub =
                                tailPush (shift - 5) idx tail subTree
                        in
                            CoreArray.set pos (SubTree newSub) tree

                    Value _ ->
                        CoreArray.push (SubTree tree) CoreArray.empty
                            |> tailPush shift idx tail

            Nothing ->
                CoreArray.push (SubTree tail) tree


calcStartShift : Int -> Int
calcStartShift len =
    if len < 1024 then
        5
    else
        (len
            |> toFloat
            |> logBase 32
            |> floor
        )
            * 5


tailPrefix : Int -> Int
tailPrefix len =
    if len < 32 then
        0
    else
        ((len - 1) `Bitwise.shiftRightLogical` 5) `Bitwise.shiftLeft` 5


{-| Return Just the element at the index or Nothing if the index is out of range.

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Nothing
-}
get : Int -> Array a -> Maybe a
get idx arr =
    if idx >= (tailPrefix arr.length) then
        case CoreArray.get (idx `Bitwise.and` 0x1F) arr.tail of
            Just x ->
                case x of
                    Value v ->
                        Just v

                    SubTree _ ->
                        Debug.crash crashMsg

            Nothing ->
                Nothing
    else
        get' arr.startShift idx arr.tree


get' : Int -> Int -> Tree a -> Maybe a
get' shift idx tree =
    let
        pos =
            hashPositionWithShift shift idx
    in
        case CoreArray.get pos tree of
            Just x ->
                case x of
                    Value v ->
                        Just v

                    SubTree subTree ->
                        get' (shift - 5) idx subTree

            Nothing ->
                Nothing


{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-}
set : Int -> a -> Array a -> Array a
set idx val arr =
    if idx < 0 || idx >= arr.length then
        arr
    else if idx >= (tailPrefix arr.length) then
        { length = arr.length
        , startShift = arr.startShift
        , tree = arr.tree
        , tail = CoreArray.set (idx `Bitwise.and` 0x1F) (Value val) arr.tail
        }
    else
        { length = arr.length
        , startShift = arr.startShift
        , tree = set' arr.startShift idx val arr.tree
        , tail = arr.tail
        }


set' : Int -> Int -> a -> Tree a -> Tree a
set' shift idx val tree =
    let
        pos =
            hashPositionWithShift shift idx
    in
        case CoreArray.get pos tree of
            Just x ->
                case x of
                    Value _ ->
                        CoreArray.set pos (Value val) tree

                    SubTree subTree ->
                        set' (shift - 5) idx val subTree

            Nothing ->
                Debug.crash crashMsg


{-| Reduce an array from the right. Read `foldr` as fold from the right.

    foldr (+) 0 (repeat 3 5) == 15
-}
foldr : (a -> b -> b) -> b -> Array a -> b
foldr f init arr =
    let
        foldr' i acc =
            case i of
                Value v ->
                    f v acc

                SubTree subTree ->
                    CoreArray.foldr foldr' acc subTree

        tail =
            CoreArray.foldr foldr' init arr.tail
    in
        CoreArray.foldr foldr' tail arr.tree


{-| Reduce an array from the left. Read `foldl` as fold from the left.

    foldl (::) [] (fromList [1,2,3]) == [3,2,1]
-}
foldl : (a -> b -> b) -> b -> Array a -> b
foldl f init arr =
    let
        foldl' i acc =
            case i of
                Value v ->
                    f v acc

                SubTree subTree ->
                    CoreArray.foldl foldl' acc subTree

        tree =
            CoreArray.foldl foldl' init arr.tree
    in
        CoreArray.foldl foldl' tree arr.tail


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
        case get idx arr of
            Just x ->
                indexedMap' mapper (push (mapper idx x) acc) (idx + 1) arr

            Nothing ->
                Debug.crash crashMsg


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
            slice' correctFrom correctTo empty arr


slice' : Int -> Int -> Array a -> Array a -> Array a
slice' from to acc arr =
    if from == to then
        acc
    else
        case get from arr of
            Just x ->
                slice' (from + 1) to (push x acc) arr

            Nothing ->
                Debug.crash crashMsg


translateIndex : Int -> Array a -> Int
translateIndex idx arr =
    let
        posIndex =
            if idx < 0 then
                arr.length + idx
            else
                idx
    in
        if posIndex < 0 then
            0
        else if posIndex >= arr.length then
            arr.length - 1
        else
            posIndex
