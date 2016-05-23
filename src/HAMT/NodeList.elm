module HAMT.NodeList exposing (..)

import Bitwise


type alias NodeList k v =
    { i0 : Node k v
    , i1 : Node k v
    , i2 : Node k v
    , i3 : Node k v
    , i4 : Node k v
    , i5 : Node k v
    , i6 : Node k v
    , i7 : Node k v
    , i8 : Node k v
    , i9 : Node k v
    , i10 : Node k v
    , i11 : Node k v
    , i12 : Node k v
    , i13 : Node k v
    , i14 : Node k v
    , i15 : Node k v
    , i16 : Node k v
    , i17 : Node k v
    , i18 : Node k v
    , i19 : Node k v
    , i20 : Node k v
    , i21 : Node k v
    , i22 : Node k v
    , i23 : Node k v
    , i24 : Node k v
    , i25 : Node k v
    , i26 : Node k v
    , i27 : Node k v
    , i28 : Node k v
    , i29 : Node k v
    , i30 : Node k v
    , i31 : Node k v
    }


type Node k v
    = Element Int k v
    | SubTree (NodeList k v)
    | Empty


empty : NodeList k v
empty =
    { i0 = Empty
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


valueByIndex : Int -> NodeList k v -> Node k v
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


setByIndex : Int -> Node k v -> NodeList k v -> NodeList k v
setByIndex idx val ls =
    case idx of
        0 ->
            { ls | i0 = val }

        1 ->
            { ls | i1 = val }

        2 ->
            { ls | i2 = val }

        3 ->
            { ls | i3 = val }

        4 ->
            { ls | i4 = val }

        5 ->
            { ls | i5 = val }

        6 ->
            { ls | i6 = val }

        7 ->
            { ls | i7 = val }

        8 ->
            { ls | i8 = val }

        9 ->
            { ls | i9 = val }

        10 ->
            { ls | i10 = val }

        11 ->
            { ls | i11 = val }

        12 ->
            { ls | i12 = val }

        13 ->
            { ls | i13 = val }

        14 ->
            { ls | i14 = val }

        15 ->
            { ls | i15 = val }

        16 ->
            { ls | i16 = val }

        17 ->
            { ls | i17 = val }

        18 ->
            { ls | i18 = val }

        19 ->
            { ls | i19 = val }

        20 ->
            { ls | i20 = val }

        21 ->
            { ls | i21 = val }

        22 ->
            { ls | i22 = val }

        23 ->
            { ls | i23 = val }

        24 ->
            { ls | i24 = val }

        25 ->
            { ls | i25 = val }

        26 ->
            { ls | i26 = val }

        27 ->
            { ls | i27 = val }

        28 ->
            { ls | i28 = val }

        29 ->
            { ls | i29 = val }

        30 ->
            { ls | i30 = val }

        31 ->
            { ls | i31 = val }

        _ ->
            Debug.crash "Index out of bounds"


hashPositionWithShift : Int -> Int -> Int
hashPositionWithShift shift hash =
    Bitwise.and (Bitwise.shiftRightLogical hash shift) 0x1F


get : Int -> Int -> k -> NodeList k v -> Node k v
get shift hash key ls =
    let
        pos =
            hashPositionWithShift shift hash

        val =
            valueByIndex pos ls
    in
        case val of
            SubTree nodes ->
                get (shift + 5) hash key nodes

            _ ->
                val


set : Int -> Int -> k -> v -> NodeList k v -> NodeList k v
set shift hash key val ls =
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
                {- OK for Arrays, bad for Dict and Set -}
                if xHash == hash then
                    setByIndex pos (Element hash key val) ls
                else
                    let
                        subNodes =
                            empty
                                |> set newShift xHash xKey xVal
                                |> set newShift hash key val
                                |> SubTree
                    in
                        setByIndex pos subNodes ls

            SubTree nodes ->
                let
                    subNodes =
                        nodes
                            |> set newShift hash key val
                            |> SubTree
                in
                    setByIndex pos subNodes ls


remove : Int -> NodeList k v -> NodeList k v
remove hash nl =
    nl
