module Leap exposing (..)

isLeapYear : Int -> Bool
isLeapYear year =
  year `rem` 4 == 0 && year `rem` 100 /= 0 || year `rem` 400 == 0