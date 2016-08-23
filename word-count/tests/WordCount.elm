module WordCount exposing (..)

import Dict exposing (Dict)
import String exposing (toLower)
import Regex exposing (Regex, replace, regex)

wordCount : String -> Dict String Int
wordCount string =
  let
    inc : Maybe Int -> Maybe Int
    inc x =
      case x of
        Just num -> Just (num + 1)
        Nothing -> Just 1

    punctuation : Regex
    punctuation = regex "[!@#$%^&*():;\"',.]+"

    words : List String
    words =
      string
      |> toLower
      |> replace Regex.All punctuation (\_ -> "")
      |> String.words

  in
    words
    |> List.foldl(\word acc -> Dict.update word inc acc) Dict.empty
