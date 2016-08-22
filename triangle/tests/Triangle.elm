module Triangle exposing (..)

import List exposing (sort)

type Triangle = Equilateral | Isosceles | Scalene

triangleKind : number -> number -> number -> Result String Triangle
triangleKind a b c =
  if a <= 0 || b <= 0 || c <= 0
    then Err "Invalid lengths"
  else if a == b && b == c && a == c
    then Ok Equilateral
  else compare' a b c

compare' : number -> number -> number -> Result String Triangle
compare' a b c =
  case sort [a, b, c] of
    x :: y :: z :: [] ->
      if x + y <= z then Err "Violates inequality"
      else if x == y || y == z then Ok Isosceles
      else Ok Scalene
    _ -> Ok Scalene

