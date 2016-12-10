module Lib.SolveMaze exposing (..)

import Lib.GridTypes exposing (..)
import Lib.GridHelpers exposing (updateCell)
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


{-|
Returns the first element that meets the requirements of the function.
-}
findInList : (a -> Bool) -> List a -> Maybe a
findInList condition list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if (condition (head)) then
                Just head
            else
                findInList condition tail


flatGrid : Grid -> List Cell
flatGrid =
    List.concat


tupleFromCell : Cell -> ( Int, Int )
tupleFromCell cell =
    ( cell.row, cell.col )


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
                    { origins =
                        acc.origins
                            |> Dict.insert (tupleFromCell cell) (Just (tupleFromCell originCell))
                    , neighbors = cell :: acc.neighbors
                    , cellsRest = acc.cellsRest
                    , endCell =
                        if (cell.category == EndCell) then
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
        if (originCell.category == StartCell && not (Dict.member (tupleFromCell originCell) originAcc)) then
            getNeighbors
                (originAcc |> Dict.insert (tupleFromCell originCell) Nothing)
                originCell
                flattenGrid
        else
            List.foldr
                extractNeighbors
                (GetNeighborsResult originAcc [] [] Nothing)
                flattenGrid


getOrigins : Grid -> Result String ( CellOrigins, Cell )
getOrigins grid =
    let
        flattenGrid =
            flatGrid grid

        initialCell =
            findInList
                (\cell -> cell.category == StartCell)
                flattenGrid
    in
        case initialCell of
            Nothing ->
                Err "Start Cell does not exist."

            Just startCell ->
                getOriginsAccumulated
                    flattenGrid
                    [ startCell ]
                    Nothing
                    Dict.empty


getOriginsAccumulated : List Cell -> List Cell -> Maybe Cell -> CellOrigins -> Result String ( CellOrigins, Cell )
getOriginsAccumulated flattenGrid queue endCell originsAcc =
    case queue of
        [] ->
            case endCell of
                Nothing ->
                    Err "End Cell was not found."

                Just cell ->
                    Ok ( originsAcc, cell )

        cell :: queueTail ->
            let
                { origins, neighbors, cellsRest, endCell } =
                    getNeighbors originsAcc cell flattenGrid
            in
                case endCell of
                    Nothing ->
                        getOriginsAccumulated
                            cellsRest
                            (queueTail ++ neighbors)
                            Nothing
                            origins

                    Just cell ->
                        Ok ( origins, cell )


getShortestPath : Cell -> CellOrigins -> Result String (List ( Int, Int ))
getShortestPath endCell origins =
    let
        endCellTuple =
            tupleFromCell endCell

        getShortestPath_ =
            \cellOrigin origins acc ->
                case cellOrigin of
                    -- The cell without origin is the initial cell
                    Nothing ->
                        Ok acc

                    Just cellTuple ->
                        case Dict.get cellTuple origins of
                            Nothing ->
                                Err "Origins data is corrupted. It should reach some cell, but it doesn't exist."

                            Just nextCellOrigin ->
                                getShortestPath_
                                    nextCellOrigin
                                    origins
                                    (cellTuple :: acc)
    in
        case Dict.get endCellTuple origins of
            Nothing ->
                Err "End Cell was not found in the origins."

            Just cellOrigin ->
                getShortestPath_
                    cellOrigin
                    origins
                    (endCellTuple :: [])


replacePathInGrid : List ( Int, Int ) -> Grid -> Grid
replacePathInGrid path grid =
    case path of
        [] ->
            grid

        ( row, col ) :: restPath ->
            replacePathInGrid
                restPath
                (updateCell grid (Cell row col AnswerCell))



-- {-|
-- If everything is ok, it will return a new grid with the new solution. Otherwise,
-- will return an error.
-- -}
-- solveMaze : Grid -> Result String Grid
-- solveMaze grid =
--     let
--
--     in
--       getOrigins grid
--         |> Result.andThen
