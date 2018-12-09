module Days.Day1 exposing (day)

import Set exposing (Set)
import Util.Day exposing (Day, Input, Solution)
import Util.RingBuffer as RingBuffer exposing (RingBuffer)


solvePart1 : Input -> Solution
solvePart1 inputString =
    String.lines inputString
        |> List.map String.toInt
        |> List.filterMap identity
        |> List.foldl (+) 0
        |> String.fromInt


solve : Int -> Set Int -> RingBuffer Int -> Int
solve currentFrequency encounteredFrequencies changes =
    let
        newCurrentFrequency =
            RingBuffer.current changes + currentFrequency
    in
    if Set.member newCurrentFrequency encounteredFrequencies then
        newCurrentFrequency

    else
        let
            newEncounteredFrequencies =
                Set.insert newCurrentFrequency encounteredFrequencies
        in
        solve newCurrentFrequency newEncounteredFrequencies (RingBuffer.roll changes)


solvePart2 : Input -> Solution
solvePart2 inputString =
    let
        currentFrequency =
            0

        encounteredFrequencies =
            Set.empty

        changes =
            String.lines inputString
                |> List.map String.toInt
                |> List.filterMap identity

        changes_ =
            case changes of
                [] ->
                    RingBuffer.fromLists [] 0 []

                x :: xs ->
                    RingBuffer.fromLists [] x xs
    in
    solve currentFrequency encounteredFrequencies changes_
        |> String.fromInt


day : Day
day =
    { number = 1
    , solvePart1 = solvePart1
    , solvePart2 = solvePart2
    }
