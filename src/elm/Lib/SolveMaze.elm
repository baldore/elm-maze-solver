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
    Dict.Dict ( Int, Int ) (Maybe ( Int, Int ))
    -> Cell
    -> List Cell
    -> ( Dict.Dict ( Int, Int ) (Maybe ( Int, Int )), List Cell, List Cell )
getNeighbors originAcc originCell flattenGrid =
    let
        extractNeighbors =
            \cell ( acc, neighbors, gridRest ) ->
                -- If it's the same, we just ignore it
                if (originCell.row == cell.row && originCell.col == cell.col) then
                    ( acc, neighbors, gridRest )
                else if (areNeighbors originCell cell) then
                    ( Dict.insert (tupleFromCell cell) (Just (tupleFromCell originCell)) acc
                    , cell :: neighbors
                    , gridRest
                    )
                else
                    ( acc, neighbors, cell :: gridRest )
    in
        -- If our cell is the starting point and doesn't exist in the origins, we
        -- need to add it, since that cell defines that the solution was found when
        -- the origins dictionary is being processed.
        if (originCell.category == StartPoint && not (Dict.member ( originCell.row, originCell.col ) originAcc)) then
            getNeighbors (Dict.insert (tupleFromCell originCell) Nothing originAcc) originCell flattenGrid
        else
            List.foldr extractNeighbors ( originAcc, [], [] ) flattenGrid


getOriginsAccumulated :
    List Cell
    -> List Cell
    -> Dict.Dict ( Int, Int ) (Maybe ( Int, Int ))
    -> Dict.Dict ( Int, Int ) (Maybe ( Int, Int ))
getOriginsAccumulated flattenGrid queue originsAcc =
    case queue of
        [] ->
            originsAcc

        cell :: queueTail ->
            let
                ( newOriginsAcc, foundNeighbors, restFlattenGrid ) =
                    getNeighbors originsAcc cell flattenGrid
            in
                getOriginsAccumulated restFlattenGrid (queueTail ++ foundNeighbors) newOriginsAcc


getOrigins : List Cell -> List Cell -> Dict.Dict ( Int, Int ) (Maybe ( Int, Int ))
getOrigins flattenGrid queue =
    getOriginsAccumulated flattenGrid queue Dict.empty
