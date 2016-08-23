module SumOfMultiples exposing (..)

import Set

sumOfMultiples : List Int -> Int -> Int
sumOfMultiples factors num =
  let
    uniq : List Int -> List Int
    uniq = Set.fromList >> Set.toList
  in
  [1..num-1]
  |> List.concatMap (\x ->
      List.map (\factor ->
        if x % factor == 0 then x else 0) factors
  )
  |> uniq
  >> List.sum
