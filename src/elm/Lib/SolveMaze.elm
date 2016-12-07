module Lib.SolveMaze exposing (..)

import Lib.GridTypes exposing (..)
import Dict


solveMaze : Grid -> List Cell
solveMaze grid =
    []


flatGrid : Grid -> List Cell
flatGrid =
    List.concat


areNeighbors : Cell -> Cell -> Bool
areNeighbors c1 c2 =
    if (c1.category == Wall || c2.category == Wall) then
        False
    else if (c1.row == c2.row) then
        abs (c1.col - c2.col) == 1
    else if (c1.col == c2.col) then
        abs (c1.row - c2.row) == 1
    else
        False


tupleFromCell : Cell -> ( Int, Int )
tupleFromCell cell =
    ( cell.row, cell.col )


getNeighbors :
    Dict.Dict ( Int, Int ) ( Int, Int )
    -> Cell
    -> List Cell
    -> ( Dict.Dict ( Int, Int ) ( Int, Int ), List Cell, List Cell )
getNeighbors acc originCell flattenGrid =
    let
        extractNeighbors =
            \cell ( acc, neighbors, gridRest ) ->
                -- If it's the same, we just ignore it
                if (originCell.row == cell.row && originCell.col == cell.col) then
                    ( acc, neighbors, gridRest )
                else if (areNeighbors originCell cell) then
                    ( Dict.insert (tupleFromCell cell) (tupleFromCell originCell) acc
                    , cell :: neighbors
                    , gridRest
                    )
                else
                    ( acc, neighbors, cell :: gridRest )
    in
        List.foldr extractNeighbors ( Dict.empty, [], [] ) flattenGrid
