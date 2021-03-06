module Lib.GridHelpers exposing (..)

import Lib.GridTypes exposing (..)


{-|
The new elements will be mapped using the index of the new element.
-}
updateListSize : (Int -> a) -> Int -> List a -> List a
updateListSize mapNewElements newSize list =
    let
        listLength =
            List.length list

        sizeDiff =
            newSize - listLength
    in
        if sizeDiff == 0 then
            list
        else if sizeDiff > 0 then
            list ++ (List.range listLength (newSize - 1) |> List.map mapNewElements)
        else
            List.take newSize list


updateCell : Grid -> Cell -> Grid
updateCell grid newCell =
    let
        replaceRow =
            \rowIndex row ->
                if rowIndex /= newCell.row then
                    row
                else
                    row |> List.indexedMap replaceCell

        replaceCell =
            \colIndex cell ->
                if colIndex /= newCell.col then
                    cell
                else
                    newCell
    in
        grid |> List.indexedMap replaceRow


updateGrid : Grid -> Rows -> Cols -> Grid
updateGrid grid rows cols =
    if rows == 0 || cols == 0 then
        []
    else
        let
            createEmptyList =
                \_ -> []

            processedRows =
                updateListSize createEmptyList rows grid
        in
            processedRows |> List.indexedMap (updateRow cols)


updateRow : Cols -> Int -> List Cell -> List Cell
updateRow cols rowIndex row =
    if cols == 0 then
        []
    else
        let
            createCellWithPos =
                \row col ->
                    { row = row
                    , col = col
                    , category =
                        -- Here is where we define the start point
                        if row == 0 && col == 0 then
                            StartCell
                        else
                            Path
                    }

            createCell =
                \colIndex -> createCellWithPos rowIndex colIndex
        in
            updateListSize createCell cols row


toggleCellCategory : Cell -> Cell
toggleCellCategory cell =
    { cell
        | category =
            case cell.category of
                Wall ->
                    Path

                Path ->
                    Wall

                other ->
                    other
    }


{-|
Set the new end cell and ensures that only one exists in the grid.
-}
setEndCellInGrid : Cell -> Grid -> Grid
setEndCellInGrid endCell grid =
    let
        mapRows =
            List.map mapCells

        mapCells =
            List.map setNewEndCell

        setNewEndCell =
            \cell ->
                if cell.category == EndCell then
                    { cell | category = Path }
                else if cell == endCell then
                    { cell | category = EndCell }
                else
                    cell
    in
        mapRows grid
