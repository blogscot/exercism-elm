module Strain exposing (..)

keep : (a -> Bool) -> List a -> List a
keep f =
  List.foldr (\x y -> if f x then x::y else y) []

discard : (a -> Bool) -> List a -> List a
discard f = keep (f >> not)
