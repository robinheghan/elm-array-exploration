module HAMT.Hash exposing (string)

import Bitwise exposing (..)
import String
import Char


string : Int -> String -> Int
string seed str =
    let
        codes =
            str
                |> String.toList
                |> List.map Char.toCode
    in
        seed
            |> hashChars codes
            |> finalize (String.length str)


hashChars : List Int -> Int -> Int
hashChars codes acc =
    if List.isEmpty codes then
        acc
    else
        let
            firstFour =
                List.take 4 codes

            rest =
                List.drop 4 codes

            hash =
                hashFourChars firstFour 0
                    |> mix acc
        in
            -- Only step if we hashed a full set of characters
            if List.length firstFour == 4 then
                hashChars rest (step hash)
            else
                hashChars rest hash


hashFourChars : List Int -> Int -> Int
hashFourChars codes shift =
    case codes of
        x :: xs ->
            ((x `and` 0xFF) `shiftLeft` shift) `or` hashFourChars xs (shift + 8)

        _ ->
            0


mix : Int -> Int -> Int
mix h1 h2 =
    let
        k1 =
            mur 0xCC9E2D51 h2

        k2 =
            (k1 `shiftLeft` 15) `or` (k1 `shiftRightLogical` 17)

        k3 =
            mur 0x1B873593 k2
    in
        h1 `Bitwise.xor` k3


mur : Int -> Int -> Int
mur c h =
    (((h `and` 0xFFFF) * c) + ((((h `shiftRightLogical` 16) * c) `and` 0xFFFF) `shiftLeft` 16)) `and` 0xFFFFFFFF


step : Int -> Int
step acc =
    let
        h1 =
            (acc `shiftLeft` 13) `or` (acc `shiftRightLogical` 19)

        h1b =
            mur 5 h1
    in
        ((h1b `and` 0xFFFF) + 0x6B64) + ((((h1b `shiftRightLogical` 16) + 0xE654) `and` 0xFFFF) `shiftLeft` 16)


finalize : Int -> Int -> Int
finalize strLength acc =
    let
        h1 =
            (acc `Bitwise.xor` strLength)

        h2 =
            h1 `Bitwise.xor` (h1 `shiftRightLogical` 16)

        h3 =
            mur 0x85EBCA6B h2

        h4 =
            h3 `Bitwise.xor` (h3 `shiftRightLogical` 13)

        h5 =
            mur 0xC2B2AE35 h4

        h6 =
            h5 `Bitwise.xor` (h5 `shiftRightLogical` 16)
    in
        h6 `shiftRightLogical` 0
