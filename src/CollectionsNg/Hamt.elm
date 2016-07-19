module CollectionsNg.Hamt
    exposing
        ( Tree
        , empty
        , hashPositionWithShift
        , get
        , set
        , remove
        , foldl
        , size
        )

import Array
import Bitwise
import List.Extra exposing (find)


type alias Tree comparable v =
    { positionMap : Int
    , blobs : Blobs comparable v
    }


type alias Blobs comparable v =
    Array.Array (Node comparable v)


type Node comparable v
    = Element Int comparable v
    | SubTree (Tree comparable v)
    | Collision Int (List ( comparable, v ))


empty : Tree comparable v
empty =
    { positionMap = 0
    , blobs = Array.empty
    }


setByIndex : Int -> Int -> Node comparable v -> Tree comparable v -> Tree comparable v
setByIndex idx blobPos val ls =
    let
        mask =
            0x01 `Bitwise.shiftLeft` idx

        alteredBitmap =
            ls.positionMap `Bitwise.or` mask

        shouldReplace =
            ls.positionMap `Bitwise.and` mask == mask

        newBlobs =
            if shouldReplace then
                Array.set blobPos val ls.blobs
            else
                insertAt blobPos val ls.blobs
    in
        { positionMap = alteredBitmap
        , blobs = newBlobs
        }


valueByIndex : Int -> Int -> Tree comparable v -> Maybe (Node comparable v)
valueByIndex idx blobPos ls =
    let
        mask =
            0x01 `Bitwise.shiftLeft` idx

        hasValue =
            ls.positionMap `Bitwise.and` mask == mask
    in
        if hasValue then
            Array.get blobPos ls.blobs
        else
            Nothing


removeByIndex : Int -> Int -> Tree comparable v -> Tree comparable v
removeByIndex idx blobPos ls =
    let
        mask =
            0x01 `Bitwise.shiftLeft` idx

        alteredBitmap =
            ls.positionMap `Bitwise.xor` mask
    in
        { positionMap = alteredBitmap
        , blobs = removeAt blobPos ls.blobs
        }


removeAt : Int -> Blobs comparable v -> Blobs comparable v
removeAt idx arr =
    let
        start =
            Array.slice 0 idx arr

        end =
            (Array.slice (idx + 1) (Array.length arr) arr)
    in
        Array.append start end


insertAt : Int -> Node comparable v -> Blobs comparable v -> Blobs comparable v
insertAt idx node arr =
    let
        start =
            Array.slice 0 idx arr

        end =
            (Array.slice idx (Array.length arr) arr)
    in
        Array.append (Array.push node start) end


hashPositionWithShift : Int -> Int -> Int
hashPositionWithShift shift hash =
    Bitwise.and 0x1F <| Bitwise.shiftRightLogical hash shift


blobPosition : Int -> Int -> Int
blobPosition idx posMap =
    countBits ((posMap `Bitwise.shiftLeft` (31 - idx)) `Bitwise.shiftLeft` 1)


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

        blobPos =
            blobPosition pos ls.positionMap
    in
        case valueByIndex pos blobPos ls of
            Nothing ->
                Nothing

            Just node ->
                case node of
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

        blobPos =
            blobPosition pos ls.positionMap

        newShift =
            shift + 5
    in
        case valueByIndex pos blobPos ls of
            Nothing ->
                setByIndex pos blobPos (Element hash key val) ls

            Just currValue ->
                case currValue of
                    Element xHash xKey xVal ->
                        if xHash == hash then
                            if xKey == key then
                                setByIndex pos blobPos (Element hash key val) ls
                            else
                                let
                                    element =
                                        if key < xKey then
                                            Collision hash [ ( key, val ), ( xKey, xVal ) ]
                                        else
                                            Collision hash [ ( xKey, xVal ), ( key, val ) ]
                                in
                                    setByIndex pos blobPos element ls
                        else
                            let
                                subNodes =
                                    set' newShift xHash xKey xVal empty
                                        |> set' newShift hash key val
                                        |> SubTree
                            in
                                setByIndex pos blobPos subNodes ls

                    Collision xHash nodes ->
                        if xHash == hash then
                            let
                                newNodes =
                                    nodes
                                        |> List.filter (\( k, _ ) -> k /= key)
                                        |> ((::) ( key, val ))
                                        |> List.sortBy fst
                            in
                                setByIndex pos blobPos (Collision hash newNodes) ls
                        else
                            let
                                collisionPos =
                                    hashPositionWithShift newShift xHash

                                collisionBlobPos =
                                    blobPosition collisionPos empty.positionMap

                                subNodes =
                                    setByIndex collisionPos collisionBlobPos currValue empty
                                        |> set' newShift hash key val
                                        |> SubTree
                            in
                                setByIndex pos blobPos subNodes ls

                    SubTree nodes ->
                        let
                            sub =
                                set' newShift hash key val nodes
                        in
                            setByIndex pos blobPos (SubTree sub) ls


remove : Int -> comparable -> Tree comparable v -> Tree comparable v
remove hash key nl =
    remove' 0 hash key nl


remove' : Int -> Int -> comparable -> Tree comparable v -> Tree comparable v
remove' shift hash key nl =
    let
        pos =
            hashPositionWithShift shift hash

        blobPos =
            blobPosition pos nl.positionMap
    in
        case valueByIndex pos blobPos nl of
            Nothing ->
                nl

            Just node ->
                case node of
                    Element _ eKey value ->
                        if eKey == key then
                            removeByIndex pos blobPos nl
                        else
                            nl

                    SubTree nodes ->
                        let
                            newSub =
                                remove' (shift + 5) hash key nodes
                        in
                            if Array.length newSub.blobs == 1 then
                                case Array.get 0 newSub.blobs of
                                    Just v ->
                                        setByIndex pos blobPos v nl

                                    Nothing ->
                                        Debug.crash "Cannot happen."
                            else
                                setByIndex pos blobPos (SubTree newSub) nl

                    Collision _ vals ->
                        let
                            newCollision =
                                List.filter (\( k, _ ) -> k /= key) vals
                        in
                            if List.length newCollision == 1 then
                                case List.head newCollision of
                                    Just ( k, v ) ->
                                        setByIndex pos blobPos (Element hash k v) nl

                                    Nothing ->
                                        Debug.crash "This should not happen."
                            else
                                setByIndex pos blobPos (Collision hash newCollision) nl


foldl : (comparable -> v -> a -> a) -> a -> Tree comparable v -> a
foldl folder acc nl =
    Array.foldl
        (\node acc ->
            case node of
                Element _ key val ->
                    folder key val acc

                SubTree nodes ->
                    foldl folder acc nodes

                Collision _ vals ->
                    let
                        colFold ( k, v ) acc =
                            folder k v acc
                    in
                        List.foldl colFold acc vals
        )
        acc
        nl.blobs


size : Tree comparable v -> Int
size nl =
    foldl (\_ _ acc -> acc + 1) 0 nl
