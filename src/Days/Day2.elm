module Days.Day2 exposing (day)

import Util.Day exposing (Day)


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


solvePart2 : List String -> Maybe ( String, String )
solvePart2 strings =
    case strings of
        [] ->
            Nothing

        str1 :: strs ->
            case find (\str2 -> hamming str1 str2 == 1) strs of
                Just match ->
                    Just ( str1, match )

                Nothing ->
                    solvePart2 strs


printPart2 =
    let
        result =
            solvePart2 parsedInput
    in
    case result of
        Nothing ->
            "Too bad"

        Just ( str1, str2 ) ->
            "(" ++ str1 ++ ", " ++ str2 ++ ")"


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


stringsContainingLetterTwice =
    input
        |> String.lines
        |> List.filter (containsNTimes 2)
        |> List.length


stringsContainingLetterThrice =
    input
        |> String.lines
        |> List.filter (containsNTimes 3)
        |> List.length


solvePart1 : Int
solvePart1 =
    stringsContainingLetterTwice * stringsContainingLetterThrice


day : Day
day =
    { number = 2
    , solvePart1 = solvePart1
    , solvePart2 = solvePart2
    }
