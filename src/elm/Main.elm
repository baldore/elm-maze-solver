module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)
import Lib.Utils exposing (updateListSize, updateCell)
import Lib.GridTypes exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { rows : Rows
    , cols : Cols
    , grid : Grid
    }


model : Model
model =
    { rows = 0
    , cols = 0
    , grid = []
    }


type Msg
    = UpdateRows String
    | UpdateCols String
    | UpdateCell Cell


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateRows rowsStr ->
            let
                rows =
                    Result.withDefault 0 (String.toInt rowsStr)
            in
                { model
                    | rows = rows
                    , grid = updateGrid model.grid rows model.cols
                }

        UpdateCols colsStr ->
            let
                cols =
                    Result.withDefault 0 (String.toInt colsStr)
            in
                { model
                    | cols = cols
                    , grid = updateGrid model.grid model.rows cols
                }

        UpdateCell cell ->
            { model
                | grid = updateCell model.grid cell
            }


{-|

If row and col are 0, then it creates the initial cell.
-}
createCellWithPos : Int -> Int -> Cell
createCellWithPos row col =
    let
        category =
            if row == 0 && col == 0 then
                StartPoint
            else
                Path
    in
        { row = row, col = col, category = category }


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
            createCell =
                \colIndex -> createCellWithPos rowIndex colIndex
        in
            updateListSize createCell cols row


getTable : Grid -> Html Msg
getTable grid =
    let
        getButtonClass =
            \cell -> "grid-button grid-button--" ++ (toString cell.category)

        makeTrs =
            grid |> List.map (\row -> tr [] (makeTds row))

        makeTds =
            List.map
                (\cell ->
                    td []
                        [ button
                            [ onClick (UpdateCell (toggleCellCategory cell))
                            , Attr.class (getButtonClass cell)
                            ]
                            []
                        ]
                )
    in
        table [ Attr.class "grid-table" ] makeTrs


view : Model -> Html Msg
view model =
    div [ Attr.class "main-container" ]
        [ h1 [] [ text "Maze Solver" ]
        , p [] [ text "Please select the rows and columns you want." ]
        , div []
            [ label []
                [ text "Rows:"
                , input
                    [ Attr.type_ "number"
                    , Attr.min "0"
                    , Attr.value (toString model.rows)
                    , onInput UpdateRows
                    ]
                    []
                ]
            , label []
                [ text "Cols:"
                , input
                    [ Attr.type_ "number"
                    , Attr.min "0"
                    , Attr.value (toString model.cols)
                    , onInput UpdateCols
                    ]
                    []
                ]
            ]
        , h2 [] [ text "Result" ]
        , getTable model.grid
        ]
