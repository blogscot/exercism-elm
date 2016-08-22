module Anagram exposing (..)

import String
import List

detect : String -> List String -> List String
detect base candidates =
  let
    normalize : String -> List String
    normalize string =
      string
      |> String.toLower
      |> String.split ""
      |> List.sort
  in
  candidates
  |> List.filter (\word ->
    normalize(word) == normalize(base) && String.toLower word /= String.toLower base)
