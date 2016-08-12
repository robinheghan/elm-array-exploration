module Hamt.JsArray
    exposing
        ( JsArray
        , empty
        , singleton
        , initialize
        , length
        , get
        , set
        , push
        , foldl
        , foldr
        , map
        , slice
        )

import Native.JsArray


type JsArray a
    = JsArray a


empty : JsArray a
empty =
    Native.JsArray.empty


singleton : a -> JsArray a
singleton =
    Native.JsArray.singleton


initialize : Int -> (Int -> a) -> JsArray a
initialize =
    Native.JsArray.initialize


length : JsArray a -> Int
length =
    Native.JsArray.length


get : Int -> JsArray a -> Maybe a
get =
    Native.JsArray.get


set : Int -> a -> JsArray a -> JsArray a
set =
    Native.JsArray.set


push : a -> JsArray a -> JsArray a
push =
    Native.JsArray.push


foldl : (a -> b -> b) -> b -> JsArray a -> b
foldl =
    Native.JsArray.foldl


foldr : (a -> b -> b) -> b -> JsArray a -> b
foldr =
    Native.JsArray.foldr


map : (a -> b) -> JsArray a -> JsArray b
map =
    Native.JsArray.map


slice : Int -> Int -> JsArray a -> JsArray a
slice =
    Native.JsArray.slice
