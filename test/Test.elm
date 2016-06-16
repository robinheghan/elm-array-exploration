module Main exposing (..)

import ElmTest exposing (..)
import Test.Hamt
import Test.Array
import Test.Dict


tests : Test
tests =
    suite "Elm Collection Tests"
        [ Test.Hamt.tests
        , Test.Array.tests
        , Test.Dict.tests
        ]


main =
    runSuite tests
