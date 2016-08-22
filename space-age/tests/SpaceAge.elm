module SpaceAge exposing (..)

type Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

ageOn : Planet -> Int -> Float
ageOn planet seconds =
  let
    earthYear : Float
    earthYear = 31557600

    ageOn' : Int -> Float -> Float
    ageOn' secs ratio = toFloat secs / ratio / earthYear
  in
  case planet of
    Earth -> ageOn' seconds 1.0
    Mercury -> ageOn' seconds 0.2408467
    Venus -> ageOn' seconds 0.61519726
    Mars -> ageOn' seconds 1.8808158
    Jupiter -> ageOn' seconds 11.862615
    Saturn -> ageOn' seconds 29.447498
    Uranus -> ageOn' seconds 84.016846
    Neptune -> ageOn' seconds 164.79132
