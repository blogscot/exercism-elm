module Hamming exposing (..)

import String exposing (length, toList)

distance : String -> String -> Maybe Int
distance strand1 strand2 =
  if length strand1 == length strand2 then
    List.map2 (\x y -> x /= y) (toList strand1) (toList strand2)
    |> List.filter identity >> List.length >> Just
  else
    Nothing
