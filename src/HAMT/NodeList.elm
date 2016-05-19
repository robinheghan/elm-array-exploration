module HAMT.NodeList exposing (..)

import Bitwise


type alias NodeList a =
    { i0 : Node a
    , i1 : Node a
    , i2 : Node a
    , i3 : Node a
    , i4 : Node a
    , i5 : Node a
    , i6 : Node a
    , i7 : Node a
    , i8 : Node a
    , i9 : Node a
    , i10 : Node a
    , i11 : Node a
    , i12 : Node a
    , i13 : Node a
    , i14 : Node a
    , i15 : Node a
    , i16 : Node a
    , i17 : Node a
    , i18 : Node a
    , i19 : Node a
    , i20 : Node a
    , i21 : Node a
    , i22 : Node a
    , i23 : Node a
    , i24 : Node a
    , i25 : Node a
    , i26 : Node a
    , i27 : Node a
    , i28 : Node a
    , i29 : Node a
    , i30 : Node a
    , i31 : Node a
    }


type Node a
    = Element Int a
    | SubTree (NodeList a)
    | Empty


empty : NodeList a
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


valueByIndex : Int -> NodeList a -> Node a
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


setByIndex : Int -> Node a -> NodeList a -> NodeList a
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


hashPositionAtDepth : Int -> Int -> Int
hashPositionAtDepth depth hash =
    Bitwise.and (Bitwise.shiftRightLogical hash (5 * depth)) 0x1F


get : Int -> Int -> NodeList a -> Node a
get hash depth ls =
    let
        pos =
            hashPositionAtDepth depth hash

        val =
            valueByIndex pos ls
    in
        case val of
            SubTree nodes ->
                get hash (depth + 1) nodes

            _ ->
                val


set : Int -> Int -> a -> NodeList a -> NodeList a
set hash depth val ls =
    let
        pos =
            hashPositionAtDepth depth hash

        currValue =
            valueByIndex pos ls
    in
        case currValue of
            Empty ->
                setByIndex pos (Element hash val) ls

            Element xHash x ->
                let
                    newDepth =
                        depth + 1

                    subNodes =
                        empty
                            |> set xHash newDepth x
                            |> set hash newDepth val
                in
                    setByIndex pos (SubTree subNodes) ls

            SubTree nodes ->
                let
                    subNodes =
                        set hash (depth + 1) val nodes
                in
                    setByIndex pos (SubTree subNodes) ls


remove : Int -> NodeList a -> NodeList a
remove hash nl =
    nl
