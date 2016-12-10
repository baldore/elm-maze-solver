module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)
import Lib.CustomEvents exposing (onRightClick)
import Lib.GridHelpers exposing (..)
import Lib.GridTypes exposing (..)
import Lib.SolveMaze exposing (solveMaze)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


type alias Model =
    { rows : Rows
    , cols : Cols
    , grid : Grid
    , controlsDisabled : Bool
    }


initialModel : Model
initialModel =
    { rows = 0
    , cols = 0
    , grid = []
    , controlsDisabled = False
    }


type Msg
    = UpdateRows String
    | UpdateCols String
    | UpdateCell Cell
    | SetEndCell Cell
    | SolveMaze
    | RestartAll


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
            if (model.controlsDisabled) then
                model
            else
                { model
                    | grid = updateCell model.grid cell
                }

        SetEndCell cell ->
            if (model.controlsDisabled) then
                model
            else
                { model
                    | grid = setEndCellInGrid cell model.grid
                }

        SolveMaze ->
            if (model.controlsDisabled) then
                model
            else
                { model
                    | controlsDisabled = True
                    , grid =
                        case solveMaze model.grid of
                            Err _ ->
                                model.grid

                            Ok solvedGrid ->
                                solvedGrid
                }

        RestartAll ->
            initialModel


getTable : Grid -> Html Msg
getTable grid =
    let
        gridVisible =
            (List.length grid > 0)
                && (List.head grid
                        |> Maybe.map (\row -> List.length row > 0)
                        |> Maybe.withDefault False
                   )

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
        if (gridVisible) then
            div []
                [ h2 [] [ text "Result" ]
                , table [ Attr.class "grid-table" ] makeTrs
                ]
        else
            text ""


view : Model -> Html Msg
view model =
    div [ Attr.class "main-container" ]
        [ h1 [] [ text "Maze Solver" ]
        , p []
            [ text """
              Please select the rows and columns you want.
              Left click in the squares to toggle between a path (white) or
              a wall (blue).
              The green square is the start point and always be located in that
              corner.
              Use right click to set the end point.
              """
            ]
        , p []
            [ button
                [ onClick SolveMaze
                , Attr.disabled model.controlsDisabled
                ]
                [ text "Show solution" ]
            , button
                [ onClick RestartAll
                ]
                [ text "Restart" ]
            ]
        , p []
            [ label []
                [ span [ Attr.class "field-label" ] [ text "Rows:" ]
                , input
                    [ Attr.type_ "number"
                    , Attr.min "0"
                    , Attr.value (toString model.rows)
                    , Attr.disabled model.controlsDisabled
                    , onInput UpdateRows
                    ]
                    []
                ]
            ]
        , p []
            [ label []
                [ span [ Attr.class "field-label" ] [ text "Cols:" ]
                , input
                    [ Attr.type_ "number"
                    , Attr.min "0"
                    , Attr.value (toString model.cols)
                    , Attr.disabled model.controlsDisabled
                    , onInput UpdateCols
                    ]
                    []
                ]
            ]
        , getTable model.grid
        ]
