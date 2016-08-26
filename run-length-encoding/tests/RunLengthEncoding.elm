module RunLengthEncoding exposing (..)

import List exposing (append, drop, head, length)
import Regex exposing (regex)
import String

encode : String -> String
encode string =
  string
  |> String.toList
  |> group_by_char
  |> List.foldl (\x acc ->
    let
      firstChar =
        Maybe.withDefault ' ' (head x)
        |> String.fromChar
    in
      acc ++ (if length x == 1
        then "" else x |> length |> toString) ++ firstChar) ""


group_by_char : List Char -> List (List Char)
group_by_char chars =
  case head chars of
    Just c ->
      let
        seq = take_while (\x -> x == c) chars
        rest = drop (length seq) chars
      in
        [seq] `append` group_by_char rest
    Nothing -> []


take_while : (Char -> Bool) -> List Char -> List Char
take_while fn chars  =
  case chars of
    x::xs -> if fn x then x :: (take_while fn xs) else []
    _ -> []


decode : String -> String
decode string =
  string
  |> Regex.split Regex.All (regex "(\\d+)(\\D)")
  |> List.filter (\x -> x /= "")
  |> decode'


decode' : List String -> String
decode' list =
  case list of
    x :: y :: rest ->
      case String.toInt x of
        Ok num ->
          String.repeat num y ++ decode' rest
        Err _ -> x ++ decode' (y :: rest)
    x :: rest -> x ++ decode' rest
    [] -> ""


version : Int
version = 2
