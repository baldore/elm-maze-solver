module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onInput)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { rows : Int
    , cols : Int
    }


model : Model
model =
    { rows = 0
    , cols = 0
    }


type Msg
    = UpdateRows String
    | UpdateCols String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateRows value ->
            { model | rows = Result.withDefault 0 (String.toInt value) }

        UpdateCols value ->
            { model | cols = Result.withDefault 0 (String.toInt value) }


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
        ]
