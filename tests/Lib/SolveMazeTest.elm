module Lib.SolveMazeTest exposing (..)

import Test exposing (..)
import Expect


all : Test
all =
    describe "Whatever"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (4 + 7) 10
            ]
        ]
