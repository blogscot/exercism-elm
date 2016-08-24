module RNATranscription exposing (..)

import String exposing (toList, join)

toRNA : String -> Result Char String
toRNA dna =
  let
    seq : List Char
    seq = toList dna

    dna_nucleotides : List Char
    dna_nucleotides = ['G', 'C', 'T', 'A']

    valid : List Char -> Bool
    valid = List.all (\chr -> List.member chr dna_nucleotides)

    transcribe : Char -> Char
    transcribe chr =
      case chr of
        'G' -> 'C'
        'C' -> 'G'
        'T' -> 'A'
        'A' -> 'U'
        _ -> chr
    in
    if valid seq then
      seq
      |> List.map (\chr -> transcribe chr |> String.fromChar)
      |> String.join ""
      |> Ok
    else
      case seq of
        x::_ -> Err x
        _ -> Err '?'
