module Lib.SolveMazeTest exposing (..)

import Test exposing (..)
import Expect
import Lib.SolveMaze exposing (..)
import Lib.GridTypes exposing (..)


pathCell : Int -> Int -> Cell
pathCell row col =
    Cell row col Path


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
                            pathCell 0 0

                        grid =
                            createGrid
                                [ "S.."
                                , "..."
                                , "..."
                                ]

                        expected =
                            [ pathCell 0 1
                            , pathCell 1 0
                            , pathCell 1 1
                            ]
                    in
                        Expect.equal
                            (getNeighbors cell grid)
                            expected
            , test "get neighbors from the bottom left corner" <|
                \() ->
                    let
                        cell =
                            pathCell 2 0

                        grid =
                            createGrid
                                [ "..."
                                , "..."
                                , "S.."
                                ]

                        expected =
                            [ pathCell 1 0
                            , pathCell 1 1
                            , pathCell 2 1
                            ]
                    in
                        Expect.equal
                            (getNeighbors cell grid)
                            expected
            , test "get neighbors from the bottom right corner" <|
                \() ->
                    let
                        cell =
                            pathCell 2 2

                        grid =
                            createGrid
                                [ "..."
                                , "..."
                                , "..S"
                                ]

                        expected =
                            [ pathCell 1 1
                            , pathCell 1 2
                            , pathCell 2 1
                            ]
                    in
                        Expect.equal
                            (getNeighbors cell grid)
                            expected
            , test "get neighbors from the top right corner" <|
                \() ->
                    let
                        cell =
                            pathCell 0 2

                        grid =
                            createGrid
                                [ "..S"
                                , "..."
                                , "..."
                                ]

                        expected =
                            [ pathCell 0 1
                            , pathCell 1 1
                            , pathCell 1 2
                            ]
                    in
                        Expect.equal
                            (getNeighbors cell grid)
                            expected
            , test "get neighbors from the center" <|
                \() ->
                    let
                        cell =
                            pathCell 2 2

                        grid =
                            createGrid
                                [ "....."
                                , "....."
                                , "..S.."
                                , "....."
                                , "....."
                                ]

                        expected =
                            [ pathCell 1 1
                            , pathCell 1 2
                            , pathCell 1 3
                            , pathCell 2 1
                            , pathCell 2 3
                            , pathCell 3 1
                            , pathCell 3 2
                            , pathCell 3 3
                            ]
                    in
                        Expect.equal
                            (getNeighbors cell grid)
                            expected
            , test "ignore walls" <|
                \() ->
                    let
                        cell =
                            pathCell 2 2

                        grid =
                            createGrid
                                [ "....."
                                , "...X."
                                , "..S.."
                                , ".X..."
                                , "....."
                                ]

                        expected =
                            [ pathCell 1 1
                            , pathCell 1 2
                            , pathCell 2 1
                            , pathCell 2 3
                            , pathCell 3 2
                            , pathCell 3 3
                            ]
                    in
                        Expect.equal
                            (getNeighbors cell grid)
                            expected
            ]
        , describe "gridToStatusCellGrid"
            [ test "should convert a grid of Cell in a grid of StatusCell" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ ".."
                                , ".."
                                ]

                        expected =
                            [ [ StatusCell (Cell 0 0 Path) Ready
                              , StatusCell (Cell 0 1 Path) Ready
                              ]
                            , [ StatusCell (Cell 1 0 Path) Ready
                              , StatusCell (Cell 1 1 Path) Ready
                              ]
                            ]
                    in
                        Expect.equal
                            (gridToStatusCellGrid grid)
                            expected
            ]
        ]
