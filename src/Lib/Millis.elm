module Lib.Millis exposing (Milliseconds, fromInt, fromSeconds, max, multiplyBy, sum, toInt, toMinutes, toSeconds, toString)


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


toMinutes : Milliseconds -> Int
toMinutes (Amount millis) =
    round ((millis |> toFloat) / 1000 / 60)


max : Milliseconds -> Milliseconds -> Milliseconds
max (Amount millis1) (Amount millis2) =
    Basics.max millis1 millis2
        |> fromInt


sum : List Milliseconds -> Milliseconds
sum list =
    list |> List.map toInt |> List.sum |> fromInt


toString : Milliseconds -> String
toString millis =
    let
        sec =
            toSeconds millis

        pad =
            String.padLeft 2 '0'

        hours =
            sec // 60 // 60

        minutes =
            remainderBy 60 (sec // 60)

        seconds =
            remainderBy 60 sec

        pos =
            if hours > 0 then
                [ String.fromInt hours
                , pad <| String.fromInt minutes
                , pad <| String.fromInt seconds
                ]

            else if minutes > 0 then
                [ String.fromInt minutes
                , pad <| String.fromInt seconds
                ]

            else
                [ String.fromInt seconds ]
    in
    String.join ":" pos
