module Array.Hamt
    exposing
        ( Array
        , empty
        , isEmpty
        , length
        , initialize
        , repeat
        , fromList
        , get
        , set
        , push
        , toList
        , toIndexedList
        , foldr
        , foldl
        , filter
        , map
        , indexedMap
        , append
        , slice
        , toString
        )

{-| Fast immutable arrays. The elements in an array must have the same type.

# Arrays
@docs Array

# Creation
@docs empty, initialize, repeat, fromList

# Query
@docs isEmpty, length, get

# Manipulate
@docs set, push, append, slice

# Lists
@docs toList, toIndexedList

# Transform
@docs foldl, foldr, filter, map, indexedMap

# Display
@docs toString
-}

import Bitwise
import Tuple
import Array.JsArray as JsArray exposing (JsArray)


{-| The array in this module is implemented as a tree with a high branching
factor (number of elements at each level). In comparision, the `Dict` has
a branching factor of 2 (left or right).

The higher the branching factor, the more elements are stored at each level.
This makes writes slower (more to copy per level), but reads faster
(fewer traversals).In practice, 32 is a good compromise.

Has to be a power of two (8, 16, 32, 64...). This is because we use the
index to tell us which path to take when navigating the tree, and we do
this by dividing it into several smaller numbers (see `shiftStep` documentation).
By dividing the index into smaller numbers, we will always get a range
which is a power of two (2 bits gives 0-3, 3 gives 0-7, 4 gives 0-15...).
-}
branchFactor : Int
branchFactor =
    32


{-| A number is made up of several bits. For bitwise operations in javascript,
numbers are treated as 32-bits integers. The number 1 is represented by 31
zeros, and a one. The important thing to take from this, is that a 32-bit integer
has enough information to represent several smaller numbers.

For a branching factor of 32, a 32-bit index has enough information to store 6
different numbers in the range of 0-31 (5 bits), and one number in the range of
0-3 (2 bits). This means that the tree of an array can have, at most, a depth
of 7.

An index essentially functions as a map. To figure out which branch to take at
any given level of the tree, we need to shift (or move) the correct amount of bits
so that those bits are at the front. We can then perform a bitwise and to read
which of the 32 branches to take.

The `shiftStep` specifices how many bits are required to represent the branching
factor.
-}
shiftStep : Int
shiftStep =
    logBase 2 (toFloat branchFactor) |> ceiling


{-| A mask which, when used in a bitwise and, reads the first `shiftStep` bits
in a number as a number of its own.
-}
bitMask : Int
bitMask =
    Bitwise.shiftRightZfBy (32 - shiftStep) 0xFFFFFFFF


{-| Representation of fast immutable arrays. You can create arrays of integers
(`Array Int`) or strings (`Array String`) or any other type of value you can
dream up.
-}
type alias Array a =
    {-
       A common operation for arrays is to push, pop, read or write elements
       at the end, also known as the tail. To avoid traversing and, in write
       cases, re-building the tree we keep the tail at the top level. Once
       the tail is full, we insert it into the tree and then reset the root
       tail.
       To retrieve (or set) an element in the array, we consult the index.
       If the index is not found in the tail, we bit shift the index with the
       `startShift` value. This gives us the index for the root level of the
       tree.
    -}
    { length : Int
    , startShift : Int
    , tree : Tree a
    , tail : JsArray a
    }


{-| Each level in the tree is represented by a `JsArray` of `Node`s.
A `Node` can either be a subtree (the next level of the tree) or, if
we're at the bottom, a `JsArray` of values (also known as a leaf).

For performance reasons we try to keep the tree as compact as possible.
This means that the tree does not necessarily have the same depth everywhere.
-}
type Node a
    = SubTree (Tree a)
    | Leaf (JsArray a)


type alias Tree a =
    JsArray (Node a)


{-| Return an empty array.

    length empty == 0
-}
empty : Array a
empty =
    {-
       `startShift` is only used when there is at least one `Node` in the `tree`.
       The minimal value is therefore equal to the `shiftStep`.
    -}
    Array 0 shiftStep JsArray.empty JsArray.empty


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
    if stop <= 0 then
        empty
    else if stop < branchFactor then
        { length = stop
        , startShift = shiftStep
        , tree = JsArray.empty
        , tail = JsArray.initialize stop 0 f
        }
    else
        let
            tailLen =
                Bitwise.and bitMask stop

            treeLen =
                stop - tailLen

            requiredTreeHeight =
                (treeLen |> toFloat |> logBase (toFloat branchFactor) |> floor)

            subTreeSize =
                branchFactor ^ requiredTreeHeight

            numberOfSubTrees =
                ceiling ((toFloat treeLen) / (toFloat subTreeSize))

            nextSubTreeSize =
                subTreeSize // branchFactor

            helper idx =
                let
                    startIndex =
                        subTreeSize * idx

                    stopIndex =
                        min (startIndex + subTreeSize) treeLen
                in
                    initializeHelp nextSubTreeSize startIndex stopIndex f
        in
            { length = stop
            , startShift = requiredTreeHeight * shiftStep
            , tree =
                JsArray.initialize numberOfSubTrees 0 helper
            , tail =
                JsArray.initialize tailLen treeLen f
            }


initializeHelp : Int -> Int -> Int -> (Int -> a) -> Node a
initializeHelp subTreeSize startIndex stopIndex f =
    let
        len =
            stopIndex - startIndex
    in
        if len == branchFactor then
            Leaf <| JsArray.initialize branchFactor startIndex f
        else
            let
                numberOfSubTrees =
                    ceiling ((toFloat len) / (toFloat subTreeSize))

                nextSubTreeSize =
                    subTreeSize // branchFactor

                helper idx =
                    let
                        start =
                            startIndex + (subTreeSize * idx)

                        stop =
                            min (start + subTreeSize) stopIndex
                    in
                        initializeHelp nextSubTreeSize start stop f
            in
                SubTree <| JsArray.initialize numberOfSubTrees 0 helper


{-| Creates an array with a given length, filled with a default element.

    repeat 5 0     == fromList [0,0,0,0,0]
    repeat 3 "cat" == fromList ["cat","cat","cat"]

Notice that `repeat 3 x` is the same as `initialize 3 (always x)`.
-}
repeat : Int -> a -> Array a
repeat n e =
    initialize n (always e)


{-| Create an array from a `List`.
-}
fromList : List a -> Array a
fromList ls =
    case ls of
        [] ->
            empty

        _ ->
            fromListHelp ls empty


fromListHelp : List a -> Array a -> Array a
fromListHelp list arr =
    let
        ( newList, newTail ) =
            JsArray.listInitialize list branchFactor

        newArray =
            pushTail newTail arr
    in
        case newList of
            [] ->
                newArray

            _ ->
                fromListHelp newList newArray


{-| Return the array represented as a string.

    (toString <| Array.fromList [1,2,3]) == "Array [1,2,3]"
-}
toString : Array a -> String
toString array =
    let
        elements =
            array
                |> map Basics.toString
                |> toList
                |> String.join ","
    in
        "Array [" ++ elements ++ "]"


{-| Return `Just` the element at the index or `Nothing`` if the index is out of range.

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Nothing
-}
get : Int -> Array a -> Maybe a
get idx arr =
    if idx < 0 || idx >= arr.length then
        Nothing
    else if idx >= tailPrefix arr.length then
        Just <| JsArray.unsafeGet (Bitwise.and bitMask idx) arr.tail
    else
        Just <| getHelp arr.startShift idx arr.tree


{-| Does not perform bounds checking.
Make sure you know the index is within bounds before using.
-}
unsafeGet : Int -> Array a -> a
unsafeGet idx arr =
    if idx >= tailPrefix arr.length then
        JsArray.unsafeGet (Bitwise.and bitMask idx) arr.tail
    else
        getHelp arr.startShift idx arr.tree


getHelp : Int -> Int -> Tree a -> a
getHelp shift idx tree =
    let
        pos =
            Bitwise.and bitMask <| Bitwise.shiftRightZfBy shift idx
    in
        case JsArray.unsafeGet pos tree of
            SubTree subTree ->
                getHelp (shift - shiftStep) idx subTree

            Leaf values ->
                JsArray.unsafeGet (Bitwise.and bitMask idx) values


{-| Set the element at a particular index. Returns an updated array.
If the index is out of range, the array is unaltered.

    set 1 7 (fromList [1,2,3]) == fromList [1,7,3]
-}
set : Int -> a -> Array a -> Array a
set idx val arr =
    if idx < 0 || idx >= arr.length then
        arr
    else if idx >= tailPrefix arr.length then
        { length = arr.length
        , startShift = arr.startShift
        , tree = arr.tree
        , tail = JsArray.unsafeSet (Bitwise.and bitMask idx) val arr.tail
        }
    else
        { length = arr.length
        , startShift = arr.startShift
        , tree = setHelp arr.startShift idx val arr.tree
        , tail = arr.tail
        }


setHelp : Int -> Int -> a -> Tree a -> Tree a
setHelp shift idx val tree =
    let
        pos =
            Bitwise.and bitMask <| Bitwise.shiftRightZfBy shift idx
    in
        case JsArray.unsafeGet pos tree of
            SubTree subTree ->
                let
                    newSub =
                        setHelp (shift - shiftStep) idx val subTree
                in
                    JsArray.unsafeSet pos (SubTree newSub) tree

            Leaf values ->
                let
                    newLeaf =
                        JsArray.unsafeSet (Bitwise.and bitMask idx) val values
                in
                    JsArray.unsafeSet pos (Leaf newLeaf) tree


{-| The reverse mask. Used to calculate the index of the first item in the tail.
-}
reverseMask : Int
reverseMask =
    Bitwise.complement bitMask


{-| Given an array length, return the index of the first element in the tail.
Used to check if a given index references something in the tail.
-}
tailPrefix : Int -> Int
tailPrefix len =
    Bitwise.and reverseMask len


{-| Push an element onto the end of an array.

    push 3 (fromList [1,2]) == fromList [1,2,3]
-}
push : a -> Array a -> Array a
push a arr =
    let
        newTail =
            JsArray.push a arr.tail
    in
        pushTail newTail arr


{-| Update the tail of an array, pushing it into the tree if necessary.
-}
pushTail : JsArray a -> Array a -> Array a
pushTail newTail arr =
    let
        tailLen =
            JsArray.length newTail

        newLen =
            arr.length + tailLen - JsArray.length arr.tail

        overflow =
            (Bitwise.shiftRightZfBy shiftStep newLen) >= (Bitwise.shiftLeftBy arr.startShift 1)

        newTree =
            if tailLen == branchFactor then
                pushTailHelp arr.startShift arr.length newTail arr.tree
            else
                arr.tree
    in
        { length = newLen
        , startShift =
            if overflow then
                arr.startShift + shiftStep
            else
                arr.startShift
        , tree =
            if overflow then
                JsArray.singleton (SubTree newTree)
            else
                newTree
        , tail =
            if tailLen == branchFactor then
                JsArray.empty
            else
                newTail
        }


pushTailHelp : Int -> Int -> JsArray a -> Tree a -> Tree a
pushTailHelp shift idx tail tree =
    let
        pos =
            Bitwise.and bitMask <| Bitwise.shiftRightZfBy shift idx
    in
        if pos >= JsArray.length tree then
            JsArray.push (Leaf tail) tree
        else
            let
                val =
                    JsArray.unsafeGet pos tree
            in
                case val of
                    SubTree subTree ->
                        let
                            newSub =
                                pushTailHelp (shift - shiftStep) idx tail subTree
                        in
                            JsArray.unsafeSet pos (SubTree newSub) tree

                    Leaf _ ->
                        let
                            newSub =
                                JsArray.singleton val
                                    |> pushTailHelp (shift - shiftStep) idx tail
                        in
                            JsArray.unsafeSet pos (SubTree newSub) tree


{-| Create a list of elements from an array.

    toList (fromList [3,5,8]) == [3,5,8]
-}
toList : Array a -> List a
toList arr =
    foldr (::) [] arr


{-| Create an indexed list from an array. Each element of the array will be
paired with its index.

    toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
-}
toIndexedList : Array a -> List ( Int, a )
toIndexedList arr =
    let
        helper n ( idx, ls ) =
            ( idx - 1, ( idx, n ) :: ls )
    in
        Tuple.second <| foldr helper ( arr.length - 1, [] ) arr


{-| Reduce an array from the right. Read `foldr` as fold from the right.

    foldr (+) 0 (repeat 3 5) == 15
-}
foldr : (a -> b -> b) -> b -> Array a -> b
foldr f init arr =
    let
        helper i acc =
            case i of
                SubTree subTree ->
                    JsArray.foldr helper acc subTree

                Leaf values ->
                    JsArray.foldr f acc values

        tail =
            JsArray.foldr f init arr.tail
    in
        JsArray.foldr helper tail arr.tree


{-| Reduce an array from the left. Read `foldl` as fold from the left.

    foldl (::) [] (fromList [1,2,3]) == [3,2,1]
-}
foldl : (a -> b -> b) -> b -> Array a -> b
foldl f init arr =
    let
        helper i acc =
            case i of
                SubTree subTree ->
                    JsArray.foldl helper acc subTree

                Leaf values ->
                    JsArray.foldl f acc values

        tree =
            JsArray.foldl helper init arr.tree
    in
        JsArray.foldl f tree arr.tail


{-| Keep only elements that satisfy the predicate.

    filter isEven (fromList [1,2,3]) == (fromList [2])
-}
filter : (a -> Bool) -> Array a -> Array a
filter f arr =
    let
        helper n acc =
            if f n then
                n :: acc
            else
                acc
    in
        foldr helper [] arr
            |> fromList


{-| Apply a function on every element in an array.

    map sqrt (fromList [1,4,9]) == fromList [1,2,3]
-}
map : (a -> b) -> Array a -> Array b
map f arr =
    let
        helper i =
            case i of
                SubTree subTree ->
                    SubTree <| JsArray.map helper subTree

                Leaf values ->
                    Leaf <| JsArray.map f values
    in
        { length = arr.length
        , startShift = arr.startShift
        , tree = JsArray.map helper arr.tree
        , tail = JsArray.map f arr.tail
        }


{-| Apply a function on every element with its index as first argument.

    indexedMap (*) (fromList [5,5,5]) == fromList [0,5,10]
-}
indexedMap : (Int -> a -> b) -> Array a -> Array b
indexedMap f arr =
    let
        helper idx =
            f idx <| unsafeGet idx arr
    in
        initialize arr.length helper


{-| Append one array onto another one.

    append (repeat 2 42) (repeat 3 81) == fromList [42,42,81,81,81]
-}
append : Array a -> Array a -> Array a
append a b =
    let
        helper i acc =
            case i of
                SubTree subTree ->
                    JsArray.foldl helper acc subTree

                Leaf values ->
                    tailMerge values acc

        tailMerge toMerge arr =
            let
                toMergeLen =
                    JsArray.length toMerge

                tailToInsert =
                    JsArray.merge arr.tail toMerge branchFactor

                tailLen =
                    JsArray.length tailToInsert

                leftOver =
                    max 0 <| (JsArray.length arr.tail) + toMergeLen - branchFactor

                newLen =
                    arr.length + toMergeLen

                overflow =
                    (Bitwise.shiftRightZfBy shiftStep newLen) >= (Bitwise.shiftLeftBy arr.startShift 1)

                newTree =
                    if tailLen == branchFactor then
                        pushTailHelp arr.startShift arr.length tailToInsert arr.tree
                    else
                        arr.tree
            in
                { length = newLen
                , startShift =
                    if overflow then
                        arr.startShift + shiftStep
                    else
                        arr.startShift
                , tree =
                    if overflow then
                        JsArray.singleton (SubTree newTree)
                    else
                        newTree
                , tail =
                    if tailLen == branchFactor then
                        JsArray.slice (toMergeLen - leftOver) toMergeLen toMerge
                    else
                        tailToInsert
                }
    in
        JsArray.foldl helper a b.tree
            |> tailMerge b.tail


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
        if correctFrom > correctTo then
            empty
        else if correctFrom > 0 then
            let
                len =
                    correctTo - correctFrom

                helper i =
                    unsafeGet (i + correctFrom) arr
            in
                initialize len helper
        else
            sliceRight correctTo arr


{-| Given a relative array index, convert it into an absolute one.

    translateIndex -1 someArray == someArray.length - 1
    translateIndex -10 someArray == someArray.length - 10
    translateIndex 5 someArray == 5
-}
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
        else if posIndex > arr.length then
            arr.length
        else
            posIndex


{-| This function is an optimization for the special case when only slicing from the right.

First, two things are tested:
1. If the array does not need slicing, return the original array.
2. If the array can be sliced by only slicing the tail, slice the tail.

In any other case we need to do three things:
1. Find the new tail in the tree, promote it to the root tail position and slice it.
2. Slice every sub tree.
3. Promote leaf nodes that are the sole element of a sub tree (for equality to work).
-}
sliceRight : Int -> Array a -> Array a
sliceRight end arr =
    if end == arr.length then
        arr
    else if end >= tailPrefix arr.length then
        { length = end
        , startShift = arr.startShift
        , tree = arr.tree
        , tail = JsArray.slice 0 (Bitwise.and bitMask end) arr.tail
        }
    else
        let
            endIdx =
                tailPrefix end

            newShift =
                if end < branchFactor then
                    shiftStep
                else
                    (end |> toFloat |> logBase (toFloat branchFactor) |> floor) * shiftStep
        in
            { length = end
            , startShift = newShift
            , tree =
                arr.tree
                    |> sliceTree arr.startShift endIdx
                    |> hoistTree arr.startShift newShift
            , tail = fetchNewTail arr.startShift end endIdx arr.tree
            }


{-| Slice and return the `Leaf` node after what is to be the last node
in the sliced tree.
-}
fetchNewTail : Int -> Int -> Int -> Tree a -> JsArray a
fetchNewTail shift end treeEnd tree =
    let
        pos =
            Bitwise.and bitMask <| Bitwise.shiftRightZfBy shift treeEnd
    in
        case JsArray.unsafeGet pos tree of
            SubTree sub ->
                fetchNewTail (shift - shiftStep) end treeEnd sub

            Leaf values ->
                JsArray.slice 0 (Bitwise.and bitMask end) values


{-| Shorten the root `Node` of the tree so it is long enough to contain
the `Node` indicated by `endIdx`. The recursively perform the same operation
to the last node of each `SubTree`.
-}
sliceTree : Int -> Int -> Tree a -> Tree a
sliceTree shift endIdx tree =
    let
        lastPos =
            Bitwise.and bitMask <| Bitwise.shiftRightZfBy shift endIdx
    in
        case JsArray.unsafeGet lastPos tree of
            SubTree sub ->
                let
                    newSub =
                        sliceTree (shift - shiftStep) endIdx sub
                in
                    case JsArray.length newSub of
                        -- The sub is empty, slice it away
                        0 ->
                            JsArray.slice 0 lastPos tree

                        -- Only contains a single element, promote it if
                        -- it is a `Leaf`. See documentation on `hoistTree`.
                        1 ->
                            let
                                val =
                                    JsArray.unsafeGet 0 newSub

                                nodeToInsert =
                                    case val of
                                        SubTree _ ->
                                            SubTree newSub

                                        Leaf _ ->
                                            val
                            in
                                tree
                                    |> JsArray.slice 0 (lastPos + 1)
                                    |> JsArray.unsafeSet lastPos nodeToInsert

                        _ ->
                            tree
                                |> JsArray.slice 0 (lastPos + 1)
                                |> JsArray.unsafeSet lastPos (SubTree newSub)

            -- This is supposed to be the new tail. Slice up to, but not including,
            -- this point.
            Leaf _ ->
                JsArray.slice 0 lastPos tree


{-| To keep the tree in its most compact form, `Leaf` nodes are stored as close to
the root as possible. To make sure that slicing does not break equality, we make
sure that this is still the case after slicing. `sliceTree` does most of the work
in this regard, but it does not handle the root node.
-}
hoistTree : Int -> Int -> Tree a -> Tree a
hoistTree oldShift newShift tree =
    if oldShift <= newShift then
        tree
    else
        case JsArray.unsafeGet 0 tree of
            SubTree sub ->
                hoistTree (oldShift - shiftStep) newShift sub

            Leaf _ ->
                tree
