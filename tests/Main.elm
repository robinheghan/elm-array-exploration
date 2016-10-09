port module Main exposing (..)

import Tests
import Test.Runner.Node exposing (run, runWithOptions)
import Json.Encode exposing (Value)


main : Program Never
main =
    (runWithOptions (Just 100) Nothing) emit Tests.all


port emit : ( String, Value ) -> Cmd msg
