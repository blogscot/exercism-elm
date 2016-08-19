module Bob exposing (..)

import String exposing (toUpper, toLower, endsWith, trim)


hey : String -> String
hey str =
  str |> trim |> check

check : String -> String
check text =
  if isShouting text then "Whoa, chill out!"
    else if isQuestion text then "Sure."
    else if text == "" then "Fine. Be that way!"
    else "Whatever."

isShouting : String -> Bool
isShouting sentence =
  (sentence |> toUpper) == sentence &&
  (sentence |> toLower) /= sentence

isQuestion : String -> Bool
isQuestion sentence =
  sentence |> endsWith("?")