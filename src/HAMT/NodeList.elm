module HAMT.NodeList exposing (..)

import Bitwise
import List.Extra exposing (find)


type alias NodeList comparable v =
    { i0 : Node comparable v
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
    Bitwise.and 0x1F <| Bitwise.shiftRightLogical hash shift


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
            Element _ _ value ->
                Just value

            SubTree nodes ->
                get' (shift + 5) hash key nodes

            Collision _ vals ->
                case find (\( k, _ ) -> k == key) vals of
                    Just ( _, value ) ->
                        Just value

                    Nothing ->
                        Nothing

            Empty ->
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
                                Collision hash
                                    [ ( key, val ), ( xKey, xVal ) ]
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
                    setByIndex pos (Collision hash (( key, val ) :: nodes)) ls
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


remove : Int -> NodeList comparable v -> NodeList comparable v
remove hash nl =
    nl
