module RunLengthEncoding exposing (..)

import List exposing (append, drop, head, length)
import Regex exposing (regex)
import String
import Char exposing (isDigit)

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
  |> compress'


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
  |> decompress'
  |> Regex.split Regex.All (regex "(\\d+)(\\D)")
  |> decode3


decode3 : List String -> String
decode3 list =
  case list of
    [] -> ""

    "" :: digit :: letter :: rest ->
      let
        num = Result.withDefault 0 (String.toInt digit)
      in
        String.repeat num letter ++ decode3 rest

    letter :: rest -> letter ++ decode3 rest


--  "12W1B12W3B24W1B" => "12WB12W3B24WB"
compress : List Char -> List Char
compress list =
  case list of
    x::y::z::rest ->
      if not (isDigit x) && y == '1' && not (isDigit z)
        then x :: compress (z :: rest)
      else x :: compress (y :: z :: rest)
    x::y::[] -> if x == '1' then [y] else list
    x :: [] -> [x]
    [] -> []

compress' : String -> String
compress' = String.toList >> compress >> String.fromList

--  "12WB12W3B24WB" => "12W1B12W3B24W1B"
decompress : List Char -> List Char
decompress list =
  case list of
    x::y::rest ->
      if not (isDigit x) && not (isDigit y)
        then x :: '1' :: decompress (y :: rest)
      else x :: decompress (y :: rest)
    x :: [] -> [x]
    [] -> []

decompress' : String -> String
decompress' = String.toList >> decompress >> String.fromList

version : Int
version = 2
