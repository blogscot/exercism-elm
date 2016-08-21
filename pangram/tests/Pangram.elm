module Pangram exposing (..)

import String

isPangram : String -> Bool
isPangram text =
  let
    alphabet = "abcdefghijklmnopqrstuvwxyz"
  in
    text
    |> String.toLower
    |> \line ->
        List.all (\letter ->
          List.member
            letter
            (line |> String.split ""))
            (alphabet |> String.split "")