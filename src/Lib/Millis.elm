module Lib.Millis exposing (Milliseconds, fromInt, fromSeconds, max, multiplyBy, sum, toInt, toSeconds)


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


max : Milliseconds -> Milliseconds -> Milliseconds
max (Amount millis1) (Amount millis2) =
    Basics.max millis1 millis2
        |> fromInt


sum : List Milliseconds -> Milliseconds
sum list =
    list |> List.map toInt |> List.sum |> fromInt
