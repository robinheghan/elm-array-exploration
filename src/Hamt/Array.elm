module Hamt.Array
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
import Hamt.JsArray as JsArray exposing (JsArray)


{-| Representation of fast immutable arrays. You can create arrays of integers
(`Array Int`) or strings (`Array String`) or any other type of value you can
dream up.
-}
type alias Array a =
    { length : Int
    , startShift : Int
    , tree : Tree a
    , tail : JsArray a
    }


type Node a
    = SubTree (Tree a)
    | Leaf (JsArray a)


type alias Tree a =
    JsArray (Node a)


crashMsg : String
crashMsg =
    "This is a bug. Please report this."


{-| Return an empty array.

    length empty == 0
-}
empty : Array a
empty =
    Array 0 5 JsArray.empty JsArray.empty


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
    else if stop < 32 then
        { length = stop
        , startShift = 5
        , tree = JsArray.empty
        , tail = JsArray.initialize stop f
        }
    else
        let
            tailLen =
                stop `Bitwise.and` 0x1F

            treeLen =
                stop - tailLen

            requiredTreeHeight =
                (treeLen |> toFloat |> logBase 32 |> floor)

            subTreeSize =
                32 ^ requiredTreeHeight

            numberOfSubTrees =
                ceiling ((toFloat treeLen) / (toFloat subTreeSize))
        in
            { length = stop
            , startShift = requiredTreeHeight * 5
            , tree =
                JsArray.initialize
                    numberOfSubTrees
                    (\idx ->
                        let
                            startIndex =
                                subTreeSize * idx

                            stopIndex =
                                min (startIndex + subTreeSize) treeLen
                        in
                            initializeTree (subTreeSize // 32) startIndex stopIndex f
                    )
            , tail =
                JsArray.initialize tailLen (\idx -> f (treeLen + idx))
            }


initializeTree : Int -> Int -> Int -> (Int -> a) -> Node a
initializeTree subTreeSize startIndex stopIndex f =
    if subTreeSize == 1 then
        Leaf <| JsArray.initialize 32 (\idx -> f (startIndex + idx))
    else
        let
            len =
                stopIndex - startIndex

            numberOfSubTrees =
                ceiling ((toFloat len) / (toFloat subTreeSize))

            nextSubTreeSize =
                subTreeSize // 32

            helper idx =
                let
                    start =
                        startIndex + (subTreeSize * idx)

                    stop =
                        min (start + subTreeSize) stopIndex
                in
                    initializeTree nextSubTreeSize start stop f
        in
            SubTree <| JsArray.initialize numberOfSubTrees helper


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
    foldr (::) [] arr


{-| Create an indexed list from an array. Each element of the array will be
paired with its index.

    toIndexedList (fromList ["cat","dog"]) == [(0,"cat"), (1,"dog")]
-}
toIndexedList : Array a -> List ( Int, a )
toIndexedList arr =
    let
        foldr' n ( idx, ls ) =
            ( idx - 1, ( idx, n ) :: ls )
    in
        snd <| foldr foldr' ( length arr - 1, [] ) arr


{-| Push an element to the end of an array.

    push 3 (fromList [1,2]) == fromList [1,2,3]
-}
push : a -> Array a -> Array a
push a arr =
    pushTree 1 (JsArray.push a arr.tail) arr


pushTree : Int -> JsArray a -> Array a -> Array a
pushTree mod newTail arr =
    let
        newLen =
            arr.length + mod

        newShift =
            calcStartShift newLen

        tailLen =
            JsArray.length newTail

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
                JsArray.singleton (SubTree newTree)
            else
                newTree
        , tail =
            if tailLen == 32 then
                JsArray.empty
            else
                newTail
        }


tailPush : Int -> Int -> JsArray a -> Tree a -> Tree a
tailPush shift idx tail tree =
    let
        pos =
            indexShift shift idx
    in
        case JsArray.get pos tree of
            Just x ->
                case x of
                    SubTree subTree ->
                        let
                            newSub =
                                tailPush (shift - 5) idx tail subTree
                        in
                            JsArray.set pos (SubTree newSub) tree

                    Leaf _ ->
                        let
                            newSub =
                                JsArray.singleton x
                                    |> tailPush (shift - 5) idx tail
                        in
                            JsArray.set pos (SubTree newSub) tree

            Nothing ->
                JsArray.push (Leaf tail) tree


calcStartShift : Int -> Int
calcStartShift len =
    if len < 1024 then
        5
    else
        (len |> toFloat |> logBase 32 |> floor) * 5


tailPrefix : Int -> Int
tailPrefix len =
    if len < 32 then
        0
    else
        ((len - 1) `Bitwise.shiftRightLogical` 5) `Bitwise.shiftLeft` 5


indexShift : Int -> Int -> Int
indexShift shift idx =
    Bitwise.and 0x1F <| Bitwise.shiftRightLogical idx shift


{-| Return Just the element at the index or Nothing if the index is out of range.

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Nothing
-}
get : Int -> Array a -> Maybe a
get idx arr =
    if idx >= tailPrefix arr.length then
        JsArray.get (idx `Bitwise.and` 0x1F) arr.tail
    else
        getRecursive arr.startShift idx arr.tree


getRecursive : Int -> Int -> Tree a -> Maybe a
getRecursive shift idx tree =
    let
        pos =
            indexShift shift idx
    in
        case JsArray.get pos tree of
            Just x ->
                case x of
                    SubTree subTree ->
                        getRecursive (shift - 5) idx subTree

                    Leaf values ->
                        JsArray.get (idx `Bitwise.and` 0x1F) values

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
        , tail = JsArray.set (idx `Bitwise.and` 0x1F) val arr.tail
        }
    else
        { length = arr.length
        , startShift = arr.startShift
        , tree = setRecursive arr.startShift idx val arr.tree
        , tail = arr.tail
        }


setRecursive : Int -> Int -> a -> Tree a -> Tree a
setRecursive shift idx val tree =
    let
        pos =
            indexShift shift idx
    in
        case JsArray.get pos tree of
            Just x ->
                case x of
                    SubTree subTree ->
                        let
                            newSub =
                                setRecursive (shift - 5) idx val subTree
                        in
                            JsArray.set pos (SubTree newSub) tree

                    Leaf values ->
                        let
                            newLeaf =
                                JsArray.set (idx `Bitwise.and` 0x1F) val values
                        in
                            JsArray.set pos (Leaf newLeaf) tree

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
                SubTree subTree ->
                    JsArray.foldr foldr' acc subTree

                Leaf values ->
                    JsArray.foldr f acc values

        tail =
            JsArray.foldr f init arr.tail
    in
        JsArray.foldr foldr' tail arr.tree


{-| Reduce an array from the left. Read `foldl` as fold from the left.

    foldl (::) [] (fromList [1,2,3]) == [3,2,1]
-}
foldl : (a -> b -> b) -> b -> Array a -> b
foldl f init arr =
    let
        foldl' i acc =
            case i of
                SubTree subTree ->
                    JsArray.foldl foldl' acc subTree

                Leaf values ->
                    JsArray.foldl f acc values

        tree =
            JsArray.foldl foldl' init arr.tree
    in
        JsArray.foldl f tree arr.tail


{-| Append two arrays to a new one.

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
                    JsArray.merge arr.tail toMerge 32

                sizeDiff =
                    (JsArray.length tailToInsert) - (JsArray.length arr.tail)

                leftOver =
                    max 0 <| (JsArray.length arr.tail) + toMergeLen - 32

                newArr =
                    pushTree sizeDiff tailToInsert arr
            in
                if leftOver == 0 then
                    newArr
                else
                    { newArr
                        | length = newArr.length + leftOver
                        , tail = JsArray.slice (toMergeLen - leftOver) toMergeLen toMerge
                    }
    in
        JsArray.foldl helper a b.tree
            |> tailMerge b.tail


{-| Keep only elements that satisfy the predicate:

    filter isEven (fromList [1..6]) == (fromList [2,4,6])
-}
filter : (a -> Bool) -> Array a -> Array a
filter f arr =
    let
        update n acc =
            if f n then
                push n acc
            else
                acc
    in
        foldl update empty arr


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
            case get idx arr of
                Just x ->
                    f idx x

                Nothing ->
                    Debug.crash crashMsg
    in
        initialize arr.length helper


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
                    case get (i + correctFrom) arr of
                        Just x ->
                            x

                        Nothing ->
                            Debug.crash crashMsg
            in
                initialize len helper
        else
            sliceRight correctTo arr


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


sliceRight : Int -> Array a -> Array a
sliceRight end arr =
    if end == arr.length then
        arr
    else if end >= tailPrefix arr.length then
        { length = end
        , startShift = arr.startShift
        , tree = arr.tree
        , tail = JsArray.slice 0 (end `Bitwise.and` 0x1F) arr.tail
        }
    else
        let
            endIdx =
                tailPrefix end

            fetchNewTail shift tree =
                case JsArray.get (indexShift shift endIdx) tree of
                    Just x ->
                        case x of
                            SubTree sub ->
                                fetchNewTail (shift - 5) sub

                            Leaf values ->
                                JsArray.slice 0 (end `Bitwise.and` 0x1F) values

                    Nothing ->
                        Debug.crash crashMsg

            sliceTree shift tree =
                let
                    lastPos =
                        indexShift shift endIdx
                in
                    case JsArray.get lastPos tree of
                        Just x ->
                            case x of
                                SubTree sub ->
                                    let
                                        newSub =
                                            sliceTree (shift - 5) sub

                                        newTree =
                                            if JsArray.length newSub == 0 then
                                                JsArray.slice 0 lastPos tree
                                            else
                                                tree
                                                    |> JsArray.slice 0 (lastPos + 1)
                                                    |> JsArray.set lastPos (SubTree newSub)
                                    in
                                        if JsArray.length newTree == 1 then
                                            case JsArray.get 0 newTree of
                                                Just x ->
                                                    case x of
                                                        SubTree y ->
                                                            y

                                                        _ ->
                                                            Debug.crash crashMsg

                                                Nothing ->
                                                    Debug.crash crashMsg
                                        else
                                            newTree

                                Leaf _ ->
                                    JsArray.slice 0 lastPos tree

                        Nothing ->
                            Debug.crash crashMsg
        in
            { length = end
            , startShift = calcStartShift end
            , tree = sliceTree arr.startShift arr.tree
            , tail = fetchNewTail arr.startShift arr.tree
            }
