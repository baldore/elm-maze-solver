module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)


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


type alias Model =
    { rows : Rows
    , cols : Cols
    }


model : Model
model =
    { rows = 0
    , cols = 0
    }


type Msg
    = UpdateRows String
    | UpdateCols String
    | SelectCell Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateRows value ->
            { model | rows = Result.withDefault 0 (String.toInt value) }

        UpdateCols value ->
            { model | cols = Result.withDefault 0 (String.toInt value) }

        _ ->
            model


getTable : Rows -> Cols -> Html Msg
getTable rows cols =
    let
        trs =
            List.range 0 (rows - 1) |> List.map (\row -> tr [] (makeTds row))

        makeTds row =
            List.range 0 (cols - 1) |> List.map (makeTd row)

        makeTd row col =
            td [] [ button [] [ text (toString ( row, col )) ] ]
    in
        table [] trs


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
        , getTable model.rows model.cols
        ]
