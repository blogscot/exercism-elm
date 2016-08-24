module RNATranscription exposing (..)

import String exposing (cons, toList)

toRNA : String -> Result Char String
toRNA dna =
  let
    transcribe : Char -> Result Char Char
    transcribe chr =
      case chr of
        'G' -> Ok 'C'
        'C' -> Ok 'G'
        'T' -> Ok 'A'
        'A' -> Ok 'U'
        _ -> Err chr
    in
    case String.uncons dna of
        Just (head, tail) ->
          Result.map2 cons (transcribe head) (toRNA tail)
        Nothing -> Ok ""
