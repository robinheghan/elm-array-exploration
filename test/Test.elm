module Main exposing (..)

import ElmTest exposing (..)
import Test.LeanArray


tests : Test
tests =
    suite "Elm Collection Tests"
        [ Test.LeanArray.tests ]


main =
    runSuite tests
