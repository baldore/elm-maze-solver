module Lib.SolveMaze exposing (..)

import Lib.GridTypes exposing (..)
import Dict


{-|
This alias defines the result of getNeighbors.

@property endCell: When the end cell is found, the maze has a solution and it
can be used to find the path until the first cell.
-}
type alias GetNeighborsResult =
    { origins : CellOrigins
    , neighbors : List Cell
    , cellsRest : List Cell
    , endCell : Maybe Cell
    }


type alias CellOrigins =
    Dict.Dict ( Int, Int ) (Maybe ( Int, Int ))


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


getNeighbors : CellOrigins -> Cell -> List Cell -> GetNeighborsResult
getNeighbors originAcc originCell flattenGrid =
    let
        extractNeighbors =
            \cell acc ->
                -- Once it finds the endCell, it stops processing the rest of the cells
                if (acc.endCell /= Nothing) then
                    acc
                else if (originCell.row == cell.row && originCell.col == cell.col) then
                    acc
                else if (areNeighbors originCell cell) then
                    { origins = Dict.insert (tupleFromCell cell) (Just (tupleFromCell originCell)) acc.origins
                    , neighbors = cell :: acc.neighbors
                    , cellsRest = acc.cellsRest
                    , endCell =
                        if (cell.category == EndPoint) then
                            Just cell
                        else
                            Nothing
                    }
                else
                    { origins = acc.origins
                    , neighbors = acc.neighbors
                    , cellsRest = cell :: acc.cellsRest
                    , endCell = Nothing
                    }
    in
        -- If our cell is the starting point and doesn't exist in the origins, we
        -- need to add it, since that cell defines that the solution was found when
        -- the origins dictionary is being processed.
        if (originCell.category == StartPoint && not (Dict.member ( originCell.row, originCell.col ) originAcc)) then
            getNeighbors (Dict.insert (tupleFromCell originCell) Nothing originAcc) originCell flattenGrid
        else
            List.foldr extractNeighbors (GetNeighborsResult originAcc [] [] Nothing) flattenGrid


getOriginsAccumulated : List Cell -> List Cell -> CellOrigins -> CellOrigins
getOriginsAccumulated flattenGrid queue originsAcc =
    case queue of
        [] ->
            originsAcc

        cell :: queueTail ->
            let
                { origins, neighbors, cellsRest, endCell } =
                    getNeighbors originsAcc cell flattenGrid
            in
                if (endCell == Nothing) then
                    getOriginsAccumulated cellsRest (queueTail ++ neighbors) origins
                else
                    origins


getOrigins : List Cell -> List Cell -> CellOrigins
getOrigins flattenGrid queue =
    getOriginsAccumulated flattenGrid queue Dict.empty
