module HAMT.NodeList
    exposing
        ( NodeList
        , empty
        , hashPositionWithShift
        , countBits
        , get
        , set
        , remove
        , foldl
        )

import Bitwise
import List.Extra exposing (find)


type alias NodeList comparable v =
    { positionMap : Int
    , i0 : Node comparable v
    , i1 : Node comparable v
    , i2 : Node comparable v
    , i3 : Node comparable v
    , i4 : Node comparable v
    , i5 : Node comparable v
    , i6 : Node comparable v
    , i7 : Node comparable v
    , i8 : Node comparable v
    , i9 : Node comparable v
    , i10 : Node comparable v
    , i11 : Node comparable v
    , i12 : Node comparable v
    , i13 : Node comparable v
    , i14 : Node comparable v
    , i15 : Node comparable v
    , i16 : Node comparable v
    , i17 : Node comparable v
    , i18 : Node comparable v
    , i19 : Node comparable v
    , i20 : Node comparable v
    , i21 : Node comparable v
    , i22 : Node comparable v
    , i23 : Node comparable v
    , i24 : Node comparable v
    , i25 : Node comparable v
    , i26 : Node comparable v
    , i27 : Node comparable v
    , i28 : Node comparable v
    , i29 : Node comparable v
    , i30 : Node comparable v
    , i31 : Node comparable v
    }


type Node comparable v
    = Element Int comparable v
    | SubTree (NodeList comparable v)
    | Collision Int (List ( comparable, v ))
    | Empty


empty : NodeList comparable v
empty =
    { positionMap = 0
    , i0 = Empty
    , i1 = Empty
    , i2 = Empty
    , i3 = Empty
    , i4 = Empty
    , i5 = Empty
    , i6 = Empty
    , i7 = Empty
    , i8 = Empty
    , i9 = Empty
    , i10 = Empty
    , i11 = Empty
    , i12 = Empty
    , i13 = Empty
    , i14 = Empty
    , i15 = Empty
    , i16 = Empty
    , i17 = Empty
    , i18 = Empty
    , i19 = Empty
    , i20 = Empty
    , i21 = Empty
    , i22 = Empty
    , i23 = Empty
    , i24 = Empty
    , i25 = Empty
    , i26 = Empty
    , i27 = Empty
    , i28 = Empty
    , i29 = Empty
    , i30 = Empty
    , i31 = Empty
    }


valueByIndex : Int -> NodeList comparable v -> Node comparable v
valueByIndex idx ls =
    case idx of
        0 ->
            ls.i0

        1 ->
            ls.i1

        2 ->
            ls.i2

        3 ->
            ls.i3

        4 ->
            ls.i4

        5 ->
            ls.i5

        6 ->
            ls.i6

        7 ->
            ls.i7

        8 ->
            ls.i8

        9 ->
            ls.i9

        10 ->
            ls.i10

        11 ->
            ls.i11

        12 ->
            ls.i12

        13 ->
            ls.i13

        14 ->
            ls.i14

        15 ->
            ls.i15

        16 ->
            ls.i16

        17 ->
            ls.i17

        18 ->
            ls.i18

        19 ->
            ls.i19

        20 ->
            ls.i20

        21 ->
            ls.i21

        22 ->
            ls.i22

        23 ->
            ls.i23

        24 ->
            ls.i24

        25 ->
            ls.i25

        26 ->
            ls.i26

        27 ->
            ls.i27

        28 ->
            ls.i28

        29 ->
            ls.i29

        30 ->
            ls.i30

        31 ->
            ls.i31

        _ ->
            Debug.crash "Index out of bounds"


setByIndex : Int -> Node comparable v -> NodeList comparable v -> NodeList comparable v
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
        case idx of
            0 ->
                { ls | positionMap = alteredBitmap, i0 = val }

            1 ->
                { ls | positionMap = alteredBitmap, i1 = val }

            2 ->
                { ls | positionMap = alteredBitmap, i2 = val }

            3 ->
                { ls | positionMap = alteredBitmap, i3 = val }

            4 ->
                { ls | positionMap = alteredBitmap, i4 = val }

            5 ->
                { ls | positionMap = alteredBitmap, i5 = val }

            6 ->
                { ls | positionMap = alteredBitmap, i6 = val }

            7 ->
                { ls | positionMap = alteredBitmap, i7 = val }

            8 ->
                { ls | positionMap = alteredBitmap, i8 = val }

            9 ->
                { ls | positionMap = alteredBitmap, i9 = val }

            10 ->
                { ls | positionMap = alteredBitmap, i10 = val }

            11 ->
                { ls | positionMap = alteredBitmap, i11 = val }

            12 ->
                { ls | positionMap = alteredBitmap, i12 = val }

            13 ->
                { ls | positionMap = alteredBitmap, i13 = val }

            14 ->
                { ls | positionMap = alteredBitmap, i14 = val }

            15 ->
                { ls | positionMap = alteredBitmap, i15 = val }

            16 ->
                { ls | positionMap = alteredBitmap, i16 = val }

            17 ->
                { ls | positionMap = alteredBitmap, i17 = val }

            18 ->
                { ls | positionMap = alteredBitmap, i18 = val }

            19 ->
                { ls | positionMap = alteredBitmap, i19 = val }

            20 ->
                { ls | positionMap = alteredBitmap, i20 = val }

            21 ->
                { ls | positionMap = alteredBitmap, i21 = val }

            22 ->
                { ls | positionMap = alteredBitmap, i22 = val }

            23 ->
                { ls | positionMap = alteredBitmap, i23 = val }

            24 ->
                { ls | positionMap = alteredBitmap, i24 = val }

            25 ->
                { ls | positionMap = alteredBitmap, i25 = val }

            26 ->
                { ls | positionMap = alteredBitmap, i26 = val }

            27 ->
                { ls | positionMap = alteredBitmap, i27 = val }

            28 ->
                { ls | positionMap = alteredBitmap, i28 = val }

            29 ->
                { ls | positionMap = alteredBitmap, i29 = val }

            30 ->
                { ls | positionMap = alteredBitmap, i30 = val }

            31 ->
                { ls | positionMap = alteredBitmap, i31 = val }

            _ ->
                Debug.crash "Index out of bounds"


hashPositionWithShift : Int -> Int -> Int
hashPositionWithShift shift hash =
    Bitwise.and 0x1F <| Bitwise.shiftRightLogical hash shift


countBits : Int -> Int
countBits bitmap =
    let
        b1 =
            bitmap - ((bitmap `Bitwise.shiftRightLogical` 1) `Bitwise.and` 0x55555555)

        b2 =
            (b1 `Bitwise.and` 0x33333333) + ((b1 `Bitwise.shiftRightLogical` 2) `Bitwise.and` 0x33333333)
    in
        (((b2 + (b2 `Bitwise.shiftRightLogical` 4)) `Bitwise.and` 0x0F0F0F0F) * 0x01010101) `Bitwise.shiftRightLogical` 24


get : Int -> comparable -> NodeList comparable v -> Maybe v
get hash key ls =
    get' 0 hash key ls


get' : Int -> Int -> comparable -> NodeList comparable v -> Maybe v
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


set : Int -> comparable -> v -> NodeList comparable v -> NodeList comparable v
set hash key val ls =
    set' 0 hash key val ls


set' : Int -> Int -> comparable -> v -> NodeList comparable v -> NodeList comparable v
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
                                [ ( key, val ), ( xKey, xVal ) ]
                                    |> List.sortBy fst
                                    |> Collision hash
                        in
                            setByIndex pos element ls
                else
                    let
                        subNodes =
                            empty
                                |> set' newShift xHash xKey xVal
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
                                |> ((::) ( key, val ))
                                |> List.sortBy fst
                    in
                        setByIndex pos (Collision hash newNodes) ls
                else
                    let
                        collisionPos =
                            hashPositionWithShift newShift xHash

                        subNodes =
                            empty
                                |> setByIndex collisionPos currValue
                                |> set' newShift hash key val
                                |> SubTree
                    in
                        setByIndex pos subNodes ls

            SubTree nodes ->
                let
                    subNodes =
                        nodes
                            |> set' newShift hash key val
                            |> SubTree
                in
                    setByIndex pos subNodes ls


remove : Int -> comparable -> NodeList comparable v -> NodeList comparable v
remove hash key nl =
    remove' 0 hash key nl


remove' : Int -> Int -> comparable -> NodeList comparable v -> NodeList comparable v
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
                        vals
                            |> List.filter (\( k, _ ) -> k /= key)
                            |> List.sortBy fst

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


getFirstValue : NodeList comparable v -> Node comparable v
getFirstValue ls =
    getFirstValue' 0 ls


getFirstValue' : Int -> NodeList comparable v -> Node comparable v
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


foldl : (comparable -> v -> a -> a) -> a -> NodeList comparable v -> a
foldl folder acc nl =
    foldl' folder acc 0 nl


foldl' : (comparable -> v -> a -> a) -> a -> Int -> NodeList comparable v -> a
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
