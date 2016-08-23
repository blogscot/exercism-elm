module SumOfMultiples exposing (..)

sumOfMultiples : List Int -> Int -> Int
sumOfMultiples factors limit =
  let
    isFactor : List Int -> Int -> Bool
    isFactor factors n =
      factors |> List.any (\x -> n % x == 0)
  in
    [1..limit-1]
    |> List.filter (factors |> isFactor)
    |> List.sum
