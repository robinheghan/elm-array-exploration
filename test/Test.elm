module Main exposing (..)

import ElmTest exposing (..)
import Test.HAMT
import Test.LeanArray


tests : Test
tests =
    suite "Elm Collection Tests"
        [ Test.HAMT.tests
        , Test.LeanArray.tests
        ]


main =
    runSuite tests
