module Main exposing (..)

import ElmTest exposing (..)
import Test.NodeList
import Test.Array


tests : Test
tests =
    suite "Elm Collection Tests"
        [ Test.NodeList.tests
        , Test.Array.tests
        ]


main =
    runSuite tests
