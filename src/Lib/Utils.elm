module Lib.Utils exposing (Device, animatedColumn, animatedEl, classifyDevice, colorToHex)

import Color
import Color.Convert
import Element exposing (..)
import Element.Font as Font
import Html.Attributes
import Simple.Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Time exposing (Weekday(..))


type alias Device =
    { class : DeviceClass
    , orientation : Orientation

    --TODO: Int statt Float? Es sind schließlich Pixel...
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



--- Animation ---


animatedUi =
    Animated.ui
        { behindContent = behindContent
        , htmlAttribute = htmlAttribute
        , html = html
        }


animatedEl : Animation -> List (Attribute msg) -> Element msg -> Element msg
animatedEl =
    animatedUi el


animatedColumn : Animation -> List (Attribute msg) -> List (Element msg) -> Element msg
animatedColumn =
    animatedUi column
