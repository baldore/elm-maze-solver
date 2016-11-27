module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { counter : Int
    }


model : Model
model =
    { counter = 0
    }


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("hola mundo genial: " ++ toString (model.counter)) ]
        , button [ onClick Increment ] [ text "Click me" ]
        ]
