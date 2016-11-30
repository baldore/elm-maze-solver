module Lib.CustomEvents exposing (..)

import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Html exposing (Attribute)


onRightClick : msg -> Attribute msg
onRightClick message =
    onWithOptions
        "contextmenu"
        { stopPropagation = True, preventDefault = True }
        (Decode.succeed message)
