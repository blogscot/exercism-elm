module Sublist exposing (..)

import List exposing (length)

version : Int
version = 2

type ListComparison = Equal | Sublist | Superlist | Unequal

sublist : List Int -> List Int -> ListComparison
sublist list1 list2 =
  if list1 == list2 then Equal
  else if list1 `contains` list2 then Superlist
  else if list2 `contains` list1 then Sublist
  else Unequal


contains : List Int -> List Int -> Bool
contains large small =
 let
  short_list = List.take (length small) large
 in
  if length large >= length small then
    if short_list == small
      then True
    else contains (List.drop 1 large) small
  else False
