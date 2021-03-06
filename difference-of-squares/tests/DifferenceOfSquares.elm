module DifferenceOfSquares exposing (..)

import List exposing (map, sum)

squareOfSum : Int -> Int
squareOfSum num =
  num |> range >> sum >> square

sumOfSquares : Int -> Int
sumOfSquares num =
  num |> range >> map square >> sum

difference : Int -> Int
difference num =
  squareOfSum num - sumOfSquares num

square : Int -> Int
square x = x^2

range : Int -> List Int
range num = [1..num]
