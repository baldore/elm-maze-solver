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


{-|
TODO: Add documentation.
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
