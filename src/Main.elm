module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)
import Debug exposing (..)
import Lib.Utils exposing (updateListSize)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Rows =
    Int


type alias Cols =
    Int


type CellType
    = Wall
    | Path


type alias Cell =
    { row : Int
    , col : Int
    , category : CellType
    }


type alias Grid =
    List (List Cell)


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
            let
                w =
                    cell |> Debug.log "cell"
            in
                model


createCellWithPos : Int -> Int -> Cell
createCellWithPos row col =
    { row = row
    , col = col
    , category = Path
    }


changeCellCategory : Cell -> Cell
changeCellCategory cell =
    { cell
        | category = Wall
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
            createRow =
                \colIndex -> createCellWithPos rowIndex colIndex
        in
            updateListSize createRow cols row


getTable : Grid -> Html Msg
getTable grid =
    let
        makeTrs =
            grid |> List.map (\row -> tr [] (makeTds row))

        makeTds =
            List.map
                (\cell ->
                    td []
                        [ button
                            [ onClick (UpdateCell cell) ]
                            [ text (toString cell) ]
                        ]
                )
    in
        table [] makeTrs


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Maze Solver" ]
        , p [] [ text "Please select the rows and columns you want." ]
        , p [] [ text ("Rows:" ++ toString model.rows) ]
        , p [] [ text ("Cols:" ++ toString model.cols) ]
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
