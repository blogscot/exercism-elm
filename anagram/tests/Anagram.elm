module Anagram exposing (..)

import String exposing (toLower, toList)

detect : String -> List String -> List String
detect base candidates =
  let
    normalize : String -> List Char
    normalize string =
      string |> toLower |> toList |> List.sort
  in
  candidates
  |> List.filter (\word ->
    normalize(word) == normalize(base) && toLower word /= toLower base)
