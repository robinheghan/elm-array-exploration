port module Main exposing (..)

import Tests
import Test.Runner.Node exposing (runWithOptions, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    (runWithOptions { runs = 500, seed = Nothing }) emit Tests.all


port emit : ( String, Value ) -> Cmd msg
