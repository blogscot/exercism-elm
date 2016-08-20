module Raindrops exposing (..)

import String

raindrops : Int -> String
raindrops x =
  let
    drops = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
  in
    List.foldr (\(a, b) acc ->
        if x % a == 0 then b :: acc else acc) [] drops
    |> (\xs ->
      if List.isEmpty xs then toString x else String.concat xs)
