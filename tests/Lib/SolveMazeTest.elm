module Lib.SolveMazeTest exposing (..)

import Test exposing (..)
import Expect
import Lib.SolveMaze as SolveMaze
import Lib.GridTypes exposing (..)


{-|
Helper for cell creation.
-}
createCell : Int -> Int -> Char -> Cell
createCell row col char =
    let
        category =
            case char of
                'S' ->
                    StartPoint

                'E' ->
                    EndPoint

                'X' ->
                    Wall

                '.' ->
                    Path

                _ ->
                    Path
    in
        Cell row col category


{-|
Creates a grid using a list of strings.

@example

[
  "Sx.",
  "...",
  "xxE"
]

Check createCell to understand how each char is converted.
-}
createGrid : List String -> Grid
createGrid chars =
    let
        createRow =
            \rowIndex row ->
                List.indexedMap
                    (\colIndex char -> createCell rowIndex colIndex char)
                    (String.toList row)
    in
        List.indexedMap createRow chars


all : Test
all =
    describe "SolveMaze"
        [ describe "getNeighbors"
            [ test "get neighbors from the top left corner" <|
                \() ->
                    let
                        cell =
                            Cell 0 0 Path

                        grid =
                            createGrid
                                [ "S.."
                                , "..."
                                , "..."
                                ]

                        expected =
                            [ Cell 0 1 Path
                            , Cell 1 0 Path
                            , Cell 1 1 Path
                            ]
                    in
                        Expect.equal
                            (SolveMaze.getNeighbors cell grid)
                            expected
            , test "get neighbors from the bottom left corner" <|
                \() ->
                    let
                        cell =
                            Cell 2 0 Path

                        grid =
                            createGrid
                                [ "..."
                                , "..."
                                , "S.."
                                ]

                        expected =
                            [ Cell 1 0 Path
                            , Cell 1 1 Path
                            , Cell 2 1 Path
                            ]
                    in
                        Expect.equal
                            (SolveMaze.getNeighbors cell grid)
                            expected
            , test "get neighbors from the bottom right corner" <|
                \() ->
                    let
                        cell =
                            Cell 2 2 Path

                        grid =
                            createGrid
                                [ "..."
                                , "..."
                                , "..S"
                                ]

                        expected =
                            [ Cell 1 1 Path
                            , Cell 1 2 Path
                            , Cell 2 1 Path
                            ]
                    in
                        Expect.equal
                            (SolveMaze.getNeighbors cell grid)
                            expected
            , test "get neighbors from the top right corner" <|
                \() ->
                    let
                        cell =
                            Cell 0 2 Path

                        grid =
                            createGrid
                                [ "..S"
                                , "..."
                                , "..."
                                ]

                        expected =
                            [ Cell 0 1 Path
                            , Cell 1 1 Path
                            , Cell 1 2 Path
                            ]
                    in
                        Expect.equal
                            (SolveMaze.getNeighbors cell grid)
                            expected
            , test "get neighbors from the center" <|
                \() ->
                    let
                        cell =
                            Cell 2 2 Path

                        grid =
                            createGrid
                                [ "....."
                                , "....."
                                , "..S.."
                                , "....."
                                , "....."
                                ]

                        expected =
                            [ Cell 1 1 Path
                            , Cell 1 2 Path
                            , Cell 1 3 Path
                            , Cell 2 1 Path
                            , Cell 2 3 Path
                            , Cell 3 1 Path
                            , Cell 3 2 Path
                            , Cell 3 3 Path
                            ]
                    in
                        Expect.equal
                            (SolveMaze.getNeighbors cell grid)
                            expected
            , test "ignore walls" <|
                \() ->
                    let
                        cell =
                            Cell 2 2 Path

                        grid =
                            createGrid
                                [ "....."
                                , "...X."
                                , "..S.."
                                , ".X..."
                                , "....."
                                ]

                        expected =
                            [ Cell 1 1 Path
                            , Cell 1 2 Path
                            , Cell 2 1 Path
                            , Cell 2 3 Path
                            , Cell 3 2 Path
                            , Cell 3 3 Path
                            ]
                    in
                        Expect.equal
                            (SolveMaze.getNeighbors cell grid)
                            expected
            ]
        ]
