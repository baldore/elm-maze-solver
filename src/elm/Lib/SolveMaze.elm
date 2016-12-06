module Lib.SolveMaze exposing (..)

import Lib.GridTypes exposing (..)


type Status
    = Ready
    | Waiting


type alias StatusCell =
    { cell : Cell
    , status : Status
    }


solveMaze : Grid -> List Cell
solveMaze grid =
    []


{-|
Returns a list with the neighbors of the cell.
-}
getNeighbors : Cell -> Grid -> List Cell
getNeighbors cell grid =
    let
        rowIncrement =
            if cell.row == 0 then
                2
            else
                1

        colIncrement =
            if cell.col == 0 then
                2
            else
                1

        filterNeighbors =
            List.filter
                (\neighbor ->
                    not (neighbor.row == cell.row && neighbor.col == cell.col)
                        && (neighbor.category /= Wall)
                )

        neighborRows =
            grid
                |> List.drop (cell.row - 1)
                |> List.take (cell.row + rowIncrement)

        getNeighborCells =
            \row ->
                row
                    |> List.drop (cell.col - 1)
                    |> List.take (cell.col + colIncrement)
    in
        neighborRows
            |> List.concatMap getNeighborCells
            |> filterNeighbors


gridToStatusCellGrid : Grid -> List (List StatusCell)
gridToStatusCellGrid grid =
    grid
        |> List.map
            (\row -> row |> List.map (\cell -> StatusCell cell Ready))
