module Lib.SolveMazeTest exposing (..)

import Test exposing (..)
import Expect


-- import Dict

import Lib.SolveMaze exposing (..)
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
        [ describe "flatGrid"
            [ test "should convert a grid in a list of cells" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ "S."
                                , ".E"
                                ]

                        expected =
                            [ Cell 0 0 StartPoint
                            , Cell 0 1 Path
                            , Cell 1 0 Path
                            , Cell 1 1 EndPoint
                            ]
                    in
                        Expect.equal
                            (flatGrid grid)
                            expected
            ]
        , describe "areNeighbors"
            [ test "should return true if the cells are neighbors" <|
                Expect.all
                    [ \() -> Expect.equal (areNeighbors (Cell 1 1 Path) (Cell 0 1 Path)) True
                    , \() -> Expect.equal (areNeighbors (Cell 1 1 Path) (Cell 1 0 Path)) True
                    , \() -> Expect.equal (areNeighbors (Cell 1 1 Path) (Cell 1 2 Path)) True
                    , \() -> Expect.equal (areNeighbors (Cell 1 1 Path) (Cell 2 1 Path)) True
                    ]
            , test "should return false if the cells are not neighbors" <|
                Expect.all
                    [ \() -> Expect.equal (areNeighbors (Cell 0 0 Path) (Cell 1 1 Path)) False
                    , \() -> Expect.equal (areNeighbors (Cell 0 0 Path) (Cell 2 0 Path)) False
                    ]
            , test "should return false if the cell is the same" <|
                \() -> Expect.equal (areNeighbors (Cell 0 0 Path) (Cell 0 0 Path)) False
            , test "should return false if the cell is a wall" <|
                \() -> Expect.equal (areNeighbors (Cell 0 0 Path) (Cell 0 1 Wall)) False
            ]
          -- , describe "getNeighbors"
          --     [ test "should return the neighbors with the origin of them and the rest of the cells" <|
          --         \() ->
          --             let
          --                 originCell =
          --                     Cell 1 1 StartPoint
          --
          --                 grid =
          --                     createGrid
          --                         [ "..."
          --                         , ".S."
          --                         , "..."
          --                         ]
          --
          --                 expected =
          --                     ( Dict.fromList
          --                         [ ( ( 0, 1 ), ( 1, 1 ) )
          --                         , ( ( 1, 0 ), ( 1, 1 ) )
          --                         , ( ( 1, 2 ), ( 1, 1 ) )
          --                         , ( ( 2, 1 ), ( 1, 1 ) )
          --                         ]
          --                     , [ Cell 0 1 Path
          --                       , Cell 1 0 Path
          --                       , Cell 1 2 Path
          --                       , Cell 2 1 Path
          --                       ]
          --                     , [ Cell 0 0 Path
          --                       , Cell 0 2 Path
          --                       , Cell 2 0 Path
          --                       , Cell 2 2 Path
          --                       ]
          --                     )
          --             in
          --                 Expect.equal
          --                     (getNeighbors Dict.empty originCell (flatGrid grid))
          --                     expected
          -- ]
        ]
