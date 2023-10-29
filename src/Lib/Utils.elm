module Lib.Utils exposing (Device, classifyDevice, colorToHex, formatSeconds)

import Color
import Color.Convert
import Element exposing (..)



--TODO: In TypedTime übernehmen


formatSeconds : Int -> String
formatSeconds sec =
    let
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


type alias Device =
    { class : DeviceClass
    , orientation : Orientation

    --TODO: die Dimensionen als Float vorhalten?
    , window : { height : Float, width : Float }
    }


classifyDevice : { height : Float, width : Float } -> Device
classifyDevice window =
    { class =
        let
            longSide =
                max window.width window.height

            shortSide =
                min window.width window.height
        in
        if shortSide < 600 then
            Phone

        else if longSide <= 1200 then
            Tablet

        else if longSide > 1200 && longSide <= 1920 then
            Desktop

        else
            BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    , window = window
    }


colorToHex : Element.Color -> String
colorToHex color =
    color
        |> toRgb
        |> Color.fromRgba
        |> Color.Convert.colorToHex
