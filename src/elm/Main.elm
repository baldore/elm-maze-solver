module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)
import Lib.Utils exposing (..)
import Lib.GridTypes exposing (..)
import Lib.CustomEvents exposing (onRightClick)


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
    | SetEndCell Cell


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

        SetEndCell cell ->
            { model
                | grid = setEndCellInGrid cell model.grid
            }


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
                            [ Attr.class (getButtonClass cell)
                            , onClick (UpdateCell (toggleCellCategory cell))
                            , onRightClick (SetEndCell cell)
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
