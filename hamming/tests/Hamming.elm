module Hamming exposing (..)

import String exposing (length, toList)

distance : String -> String -> Maybe Int
distance strand1 strand2 =
  let
    valid = length strand1 == length strand2
    s1 = strand1 |> toList
    s2 = strand2 |> toList
  in
  if valid then
    List.map2 (,) s1 s2
    |> List.map (\(x, y) -> if x /= y then 1 else 0)
    |> List.sum >> Just
  else
    Nothing
