module Lib.GridTypes exposing (..)


type alias Rows =
    Int


type alias Cols =
    Int


type CellType
    = Wall
    | Path
    | StartPoint
    | EndPoint


{-|
This type is used in the algorithm to avoid reprocessing of the same cell
-}
type CellStatus
    = Ready
    | Waiting
    | Processed


type alias Cell =
    { row : Int
    , col : Int
    , category : CellType
    }


type alias Grid =
    List (List Cell)
