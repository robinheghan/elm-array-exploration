module Main exposing (..)

import ElmTest exposing (..)
import Test.NodeList
import Test.Array
import Test.Dict


tests : Test
tests =
    suite "Elm Collection Tests"
        [ Test.NodeList.tests
        , Test.Array.tests
        , Test.Dict.tests
        ]


main =
    runSuite tests
