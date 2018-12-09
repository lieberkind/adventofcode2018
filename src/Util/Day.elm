module Util.Day exposing (Day, Input, Solution)


type alias Input =
    String


type alias Solution =
    String


type alias Day =
    { number : Int
    , solvePart1 : Input -> Solution
    , solvePart2 : Input -> Solution
    }
