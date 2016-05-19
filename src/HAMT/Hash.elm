module HAMT.Hash exposing (hash)

import HAMT.Native.Hash


hash : a -> Int
hash obj =
    Native.Hash.hash obj
