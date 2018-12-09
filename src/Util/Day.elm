module Util.Day exposing (Day, Input)


type alias Input =
    String


type alias Answer =
    String


type alias Day =
    { number : Int
    , solvePart1 : Input -> Answer
    , solvePart2 : Input -> Answer
    }
