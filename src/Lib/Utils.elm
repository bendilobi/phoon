module Lib.Utils exposing (Device, animatedColumn, animatedEl, bullet, classifyDevice, colorToHex)

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


bullet : Element msg -> Element msg
bullet content =
    row [ spacing 8, paddingXY 20 0 ]
        [ el [ alignTop, Font.bold ] <| text "•"
        , paragraph
            [ Font.alignLeft

            --- This is a bugfix for (it seems) a bug in elm-ui...
            --- See https://github.com/mdgriffith/elm-ui/issues/124
            --- Without this, the button that is overlayed on swipe in the
            --- SessionControls is not clickable at first, only on the second
            --- tap...
            -- , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
            ]
            [ content ]
        ]



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
