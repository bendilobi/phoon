module Lib.Millis exposing (Milliseconds, fromInt, fromSeconds, multiplyBy, toInt, toSeconds)


type Milliseconds
    = Amount Int


fromInt : Int -> Milliseconds
fromInt mill =
    Amount mill


toInt : Milliseconds -> Int
toInt (Amount millis) =
    millis


fromSeconds : Int -> Milliseconds
fromSeconds secs =
    Amount (secs * 1000)


multiplyBy : Int -> Milliseconds -> Milliseconds
multiplyBy factor (Amount millis) =
    Amount <| millis * factor


toSeconds : Milliseconds -> Int
toSeconds (Amount millis) =
    round ((millis |> toFloat) / 1000)



-- encoder : Milliseconds -> Json.Encode.Value
-- encoder (Amount millis) =
--     Json.Encode.int millis
