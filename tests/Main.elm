port module Main exposing (..)

import Tests
import Test.Runner.Node exposing (run, runWithOptions)
import Json.Encode exposing (Value)


main : Program Value
main =
    (runWithOptions { runs = 500, seed = Nothing }) emit Tests.all


port emit : ( String, Value ) -> Cmd msg
