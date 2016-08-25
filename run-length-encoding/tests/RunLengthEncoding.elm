module RunLengthEncoding exposing (..)

import List exposing (append, drop, head, length)
import Regex exposing (regex)
import String
import Debug

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
      acc ++ (x |> length |> toString) ++ firstChar) ""


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
  |> decode3


decode3 : List String -> String
decode3 list =
  case list of
    _ :: digit :: letter :: rest ->
      let
        _ = Debug.log "decode3" digit
        num = Result.withDefault 0 (String.toInt digit)
      in
        String.repeat num letter ++ decode3 rest
    _ -> ""


version : Int
version = 2
