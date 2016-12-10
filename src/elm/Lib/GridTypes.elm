module Lib.GridTypes exposing (..)


type alias Rows =
    Int


type alias Cols =
    Int


type CellType
    = Wall
    | Path
    | StartCell
    | EndCell
    | AnswerCell


type alias Cell =
    { row : Int
    , col : Int
    , category : CellType
    }


type alias Grid =
    List (List Cell)
