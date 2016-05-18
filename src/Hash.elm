module Hash exposing (hash)

import Native.Hash


hash : a -> Int
hash obj =
    Native.Hash.hash obj
