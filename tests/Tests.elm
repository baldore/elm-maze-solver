module Tests exposing (all)

import Test exposing (..)
import Lib.SolveMazeTest as SolveMaze


all : Test
all =
    describe "String.reverse"
        -- Nest as many descriptions as you like.
        [ SolveMaze.all
        ]
