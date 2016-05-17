module Test.LeanArray exposing (tests)

import LeanArray
import ElmTest exposing (..)


tests : Test
tests =
    suite "Array tests"
        [ test "test" <| assertEqual 1 1 ]
