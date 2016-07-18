module CollectionsNg.Hamt
    exposing
        ( Tree
        , empty
        , hashPositionWithShift
        , countBits
        , get
        , set
        , remove
        , foldl
        , size
        )

import Array
import Bitwise
import List.Extra exposing (find)

type alias Blobs comparable v =
    Array.Array (Node comparable v)

type alias Tree comparable v =
    { positionMap : Int
    , blobs : Blobs comparable v
    }


type Node comparable v
    = Empty
    | Element Int comparable v
    | SubTree (Tree comparable v)
    | Collision Int (List ( comparable, v ))


empty : Tree comparable v
empty =
    { positionMap = 0
    , blobs = Array.repeat 32 Empty
    }


valueByIndex : Int -> Tree comparable v -> Node comparable v
valueByIndex idx ls =
    case Array.get idx ls.blobs of
        Just node ->
            node
        Nothing ->
            Debug.crash "Index out of bounds"


setByIndex : Int -> Node comparable v -> Tree comparable v -> Tree comparable v
setByIndex idx val ls =
    let
        mask =
            0x01 `Bitwise.shiftLeft` idx

        alteredBitmap =
            case val of
                Empty ->
                    ls.positionMap `Bitwise.xor` mask

                _ ->
                    ls.positionMap `Bitwise.or` mask
    in
        { ls | positionMap = alteredBitmap, blobs = Array.set idx val ls.blobs}


hashPositionWithShift : Int -> Int -> Int
hashPositionWithShift shift hash =
    Bitwise.and 0x1F <| Bitwise.shiftRightLogical hash shift


{-| No idea how this works. Stole code from stack overflow.
-}
countBits : Int -> Int
countBits bitmap =
    let
        b1 =
            bitmap - ((bitmap `Bitwise.shiftRightLogical` 1) `Bitwise.and` 0x55555555)

        b2 =
            (b1 `Bitwise.and` 0x33333333) + ((b1 `Bitwise.shiftRightLogical` 2) `Bitwise.and` 0x33333333)
    in
        (((b2 + (b2 `Bitwise.shiftRightLogical` 4)) `Bitwise.and` 0x0F0F0F0F) * 0x01010101) `Bitwise.shiftRightLogical` 24


get : Int -> comparable -> Tree comparable v -> Maybe v
get hash key ls =
    get' 0 hash key ls


get' : Int -> Int -> comparable -> Tree comparable v -> Maybe v
get' shift hash key ls =
    let
        pos =
            hashPositionWithShift shift hash

        node =
            valueByIndex pos ls
    in
        case node of
            Empty ->
                Nothing

            Element _ eKey value ->
                if key == eKey then
                    Just value
                else
                    Nothing

            SubTree nodes ->
                get' (shift + 5) hash key nodes

            Collision _ vals ->
                case find (\( k, _ ) -> k == key) vals of
                    Just ( _, value ) ->
                        Just value

                    Nothing ->
                        Nothing


set : Int -> comparable -> v -> Tree comparable v -> Tree comparable v
set hash key val ls =
    set' 0 hash key val ls


set' : Int -> Int -> comparable -> v -> Tree comparable v -> Tree comparable v
set' shift hash key val ls =
    let
        pos =
            hashPositionWithShift shift hash

        currValue =
            valueByIndex pos ls

        newShift =
            shift + 5
    in
        case currValue of
            Empty ->
                setByIndex pos (Element hash key val) ls

            Element xHash xKey xVal ->
                if xHash == hash then
                    if xKey == key then
                        setByIndex pos (Element hash key val) ls
                    else
                        let
                            element =
                                if key < xKey then
                                    Collision hash [ ( key, val ), ( xKey, xVal ) ]
                                else
                                    Collision hash [ ( xKey, xVal ), ( key, val ) ]
                        in
                            setByIndex pos element ls
                else
                    let
                        subNodes =
                            set' newShift xHash xKey xVal empty
                                |> set' newShift hash key val
                                |> SubTree
                    in
                        setByIndex pos subNodes ls

            Collision xHash nodes ->
                if xHash == hash then
                    let
                        newNodes =
                            nodes
                                |> List.filter (\( k, _ ) -> k /= key)
                                |> ((++) [( key, val )])
                                |> List.sortBy fst
                    in
                        setByIndex pos (Collision hash newNodes) ls
                else
                    let
                        collisionPos =
                            hashPositionWithShift newShift xHash

                        subNodes =
                            setByIndex collisionPos currValue empty
                                |> set' newShift hash key val
                                |> SubTree
                    in
                        setByIndex pos subNodes ls

            SubTree nodes ->
                let
                    sub =
                        set' newShift hash key val nodes
                in
                    setByIndex pos (SubTree sub) ls


remove : Int -> comparable -> Tree comparable v -> Tree comparable v
remove hash key nl =
    remove' 0 hash key nl


remove' : Int -> Int -> comparable -> Tree comparable v -> Tree comparable v
remove' shift hash key nl =
    let
        pos =
            hashPositionWithShift shift hash

        node =
            valueByIndex pos nl
    in
        case node of
            Empty ->
                nl

            Element _ eKey value ->
                if eKey == key then
                    setByIndex pos Empty nl
                else
                    nl

            SubTree nodes ->
                let
                    newSub =
                        remove' (shift + 5) hash key nodes
                in
                    if countBits newSub.positionMap == 1 then
                        setByIndex pos (getFirstValue newSub) nl
                    else
                        setByIndex pos (SubTree newSub) nl

            Collision _ vals ->
                let
                    newCollision =
                        List.filter (\( k, _ ) -> k /= key) vals

                    newLength =
                        List.length newCollision
                in
                    if newLength == 1 then
                        case List.head newCollision of
                            Just ( k, v ) ->
                                setByIndex pos (Element hash k v) nl

                            Nothing ->
                                Debug.crash "This should not happen."
                    else
                        setByIndex pos (Collision hash newCollision) nl


getFirstValue : Tree comparable v -> Node comparable v
getFirstValue ls =
    getFirstValue' 0 ls


getFirstValue' : Int -> Tree comparable v -> Node comparable v
getFirstValue' pos ls =
    if pos > 31 then
        Empty
    else
        let
            node =
                valueByIndex pos ls
        in
            case node of
                Empty ->
                    getFirstValue' (pos + 1) ls

                _ ->
                    node


foldl : (comparable -> v -> a -> a) -> a -> Tree comparable v -> a
foldl folder acc nl =
    foldl' folder acc 0 nl


foldl' : (comparable -> v -> a -> a) -> a -> Int -> Tree comparable v -> a
foldl' folder acc pos nl =
    if pos > 31 then
        acc
    else
        case valueByIndex pos nl of
            Empty ->
                foldl' folder acc (pos + 1) nl

            Element _ key val ->
                foldl' folder (folder key val acc) (pos + 1) nl

            SubTree nodes ->
                foldl' folder (foldl folder acc nodes) (pos + 1) nl

            Collision _ vals ->
                let
                    colFold ( k, v ) acc =
                        folder k v acc
                in
                    foldl' folder (List.foldl colFold acc vals) (pos + 1) nl


size : Tree comparable v -> Int
size nl =
    foldl (\_ _ acc -> acc + 1) 0 nl
