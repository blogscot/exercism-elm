module WordCount exposing (..)

import Dict exposing (Dict, update)
import String exposing (toLower)
import Regex exposing (Regex, replace, regex)

wordCount : String -> Dict String Int
wordCount string =
  let
    inc : Maybe Int -> Maybe Int
    inc = Maybe.withDefault 0 >> (+) 1 >> Just

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
    |> List.foldl (\word acc -> update word inc acc) Dict.empty
