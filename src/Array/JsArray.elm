module Array.JsArray
    exposing
        ( JsArray
        , empty
        , singleton
        , initialize
        , statefulInitialize
        , length
        , get
        , unsafeGet
        , set
        , push
        , foldl
        , foldr
        , map
        , slice
        , merge
        )

import Native.JsArray


type JsArray a
    = JsArray a


type alias State a b =
    ( Maybe a, b )


empty : JsArray a
empty =
    Native.JsArray.empty


singleton : a -> JsArray a
singleton =
    Native.JsArray.singleton


initialize : Int -> Int -> (Int -> a) -> JsArray a
initialize =
    Native.JsArray.initialize


statefulInitialize : State a b -> (State a b -> State a b) -> Int -> ( State a b, JsArray a )
statefulInitialize =
    Native.JsArray.statefulInit


length : JsArray a -> Int
length =
    Native.JsArray.length


get : Int -> JsArray a -> Maybe a
get =
    Native.JsArray.get


unsafeGet : Int -> JsArray a -> a
unsafeGet =
    Native.JsArray.unsafeGet


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


merge : JsArray a -> JsArray a -> Int -> JsArray a
merge =
    Native.JsArray.merge
