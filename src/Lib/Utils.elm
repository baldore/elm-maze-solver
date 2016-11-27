module Lib.Utils exposing (..)


getFromList : Int -> List a -> Maybe a
getFromList index list =
    if index < 0 then
        Nothing
    else
        case List.drop index list of
            [] ->
                Nothing

            head :: _ ->
                Just head
