module Lib.SolveMaze exposing (..)

import Lib.GridTypes exposing (..)


{-|
Returns a list with the neighbors of the cell.
-}
getNeighbors : Cell -> Grid -> List Cell
getNeighbors cell grid =
    let
        filterNeighbors =
            List.filter
                (\neighbor ->
                    not (neighbor.row == cell.row && neighbor.col == cell.col)
                )

        neighborRows =
            grid
                |> List.drop (cell.row - 1)
                |> List.take (cell.row + 2)

        getNeighborCells =
            \row ->
                row |> List.drop (cell.col - 1) |> List.take (cell.col + 2)
    in
        neighborRows
            |> List.concatMap getNeighborCells
            |> filterNeighbors


solveMaze : Grid -> List Cell
solveMaze grid =
    []
