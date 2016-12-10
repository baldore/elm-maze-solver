module Lib.SolveMazeTest exposing (..)

import Test exposing (..)
import Expect
import Dict
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
                    StartCell

                'E' ->
                    EndCell

                'X' ->
                    Wall

                '.' ->
                    Path

                'a' ->
                    AnswerCell

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
                    Expect.equal
                        (createGrid
                            [ "S."
                            , ".E"
                            ]
                            |> flatGrid
                        )
                        [ Cell 0 0 StartCell
                        , Cell 0 1 Path
                        , Cell 1 0 Path
                        , Cell 1 1 EndCell
                        ]
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
        , describe "getNeighbors"
            [ test "should return the neighbors with the origin of them and the rest of the cells" <|
                \() ->
                    let
                        originCell =
                            Cell 1 1 StartCell

                        flattenGrid =
                            createGrid
                                [ "..."
                                , ".S."
                                , "..."
                                ]
                                |> flatGrid

                        expected =
                            { origins =
                                Dict.fromList
                                    [ ( ( 1, 1 ), Nothing )
                                    , ( ( 0, 1 ), Just ( 1, 1 ) )
                                    , ( ( 1, 0 ), Just ( 1, 1 ) )
                                    , ( ( 1, 2 ), Just ( 1, 1 ) )
                                    , ( ( 2, 1 ), Just ( 1, 1 ) )
                                    ]
                            , neighbors =
                                [ Cell 0 1 Path
                                , Cell 1 0 Path
                                , Cell 1 2 Path
                                , Cell 2 1 Path
                                ]
                            , cellsRest =
                                [ Cell 0 0 Path
                                , Cell 0 2 Path
                                , Cell 2 0 Path
                                , Cell 2 2 Path
                                ]
                            , endCell = Nothing
                            }
                    in
                        Expect.equal (getNeighbors Dict.empty originCell flattenGrid) expected
            , test "should return empty values if there's no neighbors" <|
                \() ->
                    let
                        originCell =
                            Cell 0 0 Path

                        flattenGrid =
                            createGrid
                                [ ".X."
                                , "XX."
                                , "..."
                                ]
                                |> flatGrid

                        expected =
                            { origins = Dict.empty
                            , neighbors = []
                            , cellsRest =
                                [ Cell 0 1 Wall
                                , Cell 0 2 Path
                                , Cell 1 0 Wall
                                , Cell 1 1 Wall
                                , Cell 1 2 Path
                                , Cell 2 0 Path
                                , Cell 2 1 Path
                                , Cell 2 2 Path
                                ]
                            , endCell = Nothing
                            }
                    in
                        Expect.equal (getNeighbors Dict.empty originCell flattenGrid) expected
            , test "should set the endCell if it was found" <|
                \() ->
                    let
                        originCell =
                            Cell 0 0 StartCell

                        flattenGrid =
                            createGrid
                                [ "SE."
                                , "..."
                                ]
                                |> flatGrid

                        expected =
                            { origins =
                                Dict.fromList
                                    [ ( ( 0, 0 ), Nothing )
                                    , ( ( 0, 1 ), Just ( 0, 0 ) )
                                    , ( ( 1, 0 ), Just ( 0, 0 ) )
                                    ]
                            , neighbors =
                                [ Cell 0 1 EndCell
                                , Cell 1 0 Path
                                ]
                            , cellsRest =
                                [ Cell 0 2 Path
                                , Cell 1 1 Path
                                , Cell 1 2 Path
                                ]
                            , endCell = Just (Cell 0 1 EndCell)
                            }
                    in
                        Expect.equal (getNeighbors Dict.empty originCell flattenGrid) expected
            ]
        , describe "getOrigins"
            [ test "should return the origins of the whole grid" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ "S.."
                                , "..."
                                , "..E"
                                ]

                        expected =
                            Ok
                                ( Dict.fromList
                                    [ ( ( 0, 0 ), Nothing )
                                    , ( ( 0, 1 ), Just ( 0, 0 ) )
                                    , ( ( 1, 0 ), Just ( 0, 0 ) )
                                    , ( ( 0, 2 ), Just ( 0, 1 ) )
                                    , ( ( 1, 1 ), Just ( 0, 1 ) )
                                    , ( ( 2, 0 ), Just ( 1, 0 ) )
                                    , ( ( 1, 2 ), Just ( 0, 2 ) )
                                    , ( ( 2, 1 ), Just ( 1, 1 ) )
                                    , ( ( 2, 2 ), Just ( 1, 2 ) )
                                    ]
                                , Cell 2 2 EndCell
                                )
                    in
                        Expect.equal (getOrigins grid) expected
            , test "should stop once endCell is found" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ "S.."
                                , "E.."
                                , "..."
                                ]

                        expected =
                            Ok
                                ( Dict.fromList
                                    [ ( ( 0, 0 ), Nothing )
                                    , ( ( 1, 0 ), Just ( 0, 0 ) )
                                    ]
                                , Cell 1 0 EndCell
                                )
                    in
                        Expect.equal (getOrigins grid) expected
            , test "should return an error if the startCell was not found" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ "..."
                                , "..."
                                , "..."
                                ]

                        expected =
                            Err "Start Cell does not exist."
                    in
                        Expect.equal (getOrigins grid) expected
            , test "should return an error if the endCell was not found" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ "S.."
                                , "..."
                                , "..."
                                ]

                        expected =
                            Err "End Cell was not found."
                    in
                        Expect.equal (getOrigins grid) expected
            ]
        , describe "getShortestPath"
            [ test "should return an error if the end cell is not present in the origins" <|
                \() ->
                    let
                        endCell =
                            Cell 0 4 EndCell

                        origins =
                            Dict.fromList
                                [ ( ( 0, 0 ), Nothing )
                                , ( ( 0, 1 ), Just ( 0, 0 ) )
                                ]

                        expected =
                            Err "End Cell was not found in the origins."
                    in
                        Expect.equal (getShortestPath endCell origins) expected
            , test "should return the correct path if the values are good" <|
                \() ->
                    let
                        endCell =
                            Cell 0 3 EndCell

                        origins =
                            Dict.fromList
                                [ ( ( 0, 0 ), Nothing )
                                , ( ( 0, 1 ), Just ( 0, 0 ) )
                                , ( ( 0, 2 ), Just ( 0, 1 ) )
                                , ( ( 0, 3 ), Just ( 0, 2 ) )
                                ]

                        expected =
                            Ok [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ) ]
                    in
                        Expect.equal (getShortestPath endCell origins) expected
            , test "should return an error if during the origin cell look up, the next origin doesnt exist" <|
                \() ->
                    let
                        endCell =
                            Cell 0 3 EndCell

                        origins =
                            Dict.fromList
                                [ ( ( 0, 0 ), Nothing )
                                , ( ( 0, 1 ), Just ( 0, 0 ) )
                                  -- (0, 7) doesn't exist and breaks the test
                                , ( ( 0, 2 ), Just ( 0, 7 ) )
                                , ( ( 0, 3 ), Just ( 0, 2 ) )
                                ]

                        expected =
                            Err "Origins data is corrupted. It should reach some cell, but it doesn't exist."
                    in
                        Expect.equal (getShortestPath endCell origins) expected
            ]
        , describe "replacePathInGrid"
            [ test "should replace all the elements in path on the grid" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ "SX."
                                , ".X."
                                , "EX."
                                ]

                        path =
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 2, 0 )
                            ]

                        expected =
                            createGrid
                                [ "aX."
                                , "aX."
                                , "aX."
                                ]
                    in
                        Expect.equal (replacePathInGrid path grid) expected
            ]
        , describe "solveMaze"
            [ test "should return a new grid with the resolved path" <|
                \() ->
                    let
                        grid =
                            createGrid
                                [ "SX."
                                , ".X."
                                , "EX."
                                ]

                        expected =
                            createGrid
                                [ "aX."
                                , "aX."
                                , "aX."
                                ]
                                |> Ok
                    in
                        Expect.equal (solveMaze grid) expected
            ]
        ]
