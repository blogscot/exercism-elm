module Raindrops exposing (..)

import String

raindrops : Int -> String
raindrops num =
  raindrops' num ""

raindrops' : Int -> String -> String
raindrops' num acc =
  let
    rem_three = num `rem` 3
    rem_five = num `rem` 5
    rem_seven = num `rem` 7
  in
  if num == 0 then acc
    else if rem_three == 0 then raindrops' (shake num 3) (acc ++ "Pling")
    else if rem_five == 0 then raindrops' (shake num 5) (acc ++ "Plang")
    else if rem_seven == 0 then raindrops' (shake num 7) (acc ++ "Plong")
    else if String.isEmpty acc then toString num
      else acc


-- Reduces number until it contains no more factors
-- ie.
shake : Int -> Int -> Int
shake num factor =
  let
    remainder = num `rem` factor
  in
  if num == 1 then 0
    else if remainder == 0 then shake (num // factor) factor
    else num