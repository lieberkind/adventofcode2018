module Days.Day2 exposing (day)

import Dict exposing (Dict)
import Util.Day exposing (Day, Input, Solution)


parseInput : String -> List String
parseInput str =
    String.lines str


find : (a -> Bool) -> List a -> Maybe a
find predicate ls =
    case ls of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just x

            else
                find predicate xs


{-| It's assumed that the strings are of the same length
-}
hamming : String -> String -> Int
hamming str1 str2 =
    List.map2 (==) (String.toList str1) (String.toList str2)
        |> List.filter not
        |> List.length


removeDifferences : String -> String -> String
removeDifferences str1 str2 =
    List.map2
        (\a b ->
            if a == b then
                Just a

            else
                Nothing
        )
        (String.toList str1)
        (String.toList str2)
        |> List.filterMap identity
        |> String.fromList


solvePart2Help : List String -> String
solvePart2Help strings =
    case strings of
        [] ->
            "No cigar"

        str1 :: strs ->
            case find (\str2 -> hamming str1 str2 == 1) strs of
                Just match ->
                    removeDifferences str1 match

                Nothing ->
                    solvePart2Help strs


solvePart2 : Input -> Solution
solvePart2 input =
    solvePart2Help (String.lines input)


containsNTimes : Int -> String -> Bool
containsNTimes n string =
    string
        |> String.foldl countUp Dict.empty
        |> Dict.values
        |> List.any (\count -> count == n)


countUp : comparable -> Dict comparable Int -> Dict comparable Int
countUp key counts =
    if Dict.member key counts then
        Dict.update key (Maybe.map inc) counts

    else
        Dict.insert key 1 counts


inc : Int -> Int
inc =
    (+) 1


stringsContainingLetterTwice : String -> Int
stringsContainingLetterTwice input =
    input
        |> String.lines
        |> List.filter (containsNTimes 2)
        |> List.length


stringsContainingLetterThrice : String -> Int
stringsContainingLetterThrice input =
    input
        |> String.lines
        |> List.filter (containsNTimes 3)
        |> List.length


solvePart1 : Input -> Solution
solvePart1 input =
    (stringsContainingLetterTwice input * stringsContainingLetterThrice input)
        |> String.fromInt


day : Day
day =
    { number = 2
    , solvePart1 = solvePart1
    , solvePart2 = solvePart2
    }
