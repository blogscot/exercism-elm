module Bob exposing (..)

import String exposing (toUpper, toLower, endsWith, trim, isEmpty)

hey : String -> String
hey msg =
  let
    isShouting msg =
      toUpper msg == msg && toLower msg /= msg
  in
    if isShouting msg then "Whoa, chill out!"
      else if msg |> endsWith("?") then "Sure."
      else if msg |> trim |> isEmpty then "Fine. Be that way!"
      else "Whatever."
