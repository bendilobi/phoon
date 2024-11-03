module Lib.Utils exposing (Device, MainTask(..), animatedColumn, animatedEl, classifyDevice, colorToHex, mainTaskIcon)

import Color
import Color.Convert
import Element exposing (..)
import FeatherIcons
import Simple.Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Svg
import Svg.Attributes
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


type MainTask
    = Motivate
    | Practice
    | Optimize


mainTaskIcon : Bool -> MainTask -> FeatherIcons.Icon
mainTaskIcon filled task =
    case ( task, filled ) of
        ( Motivate, False ) ->
            FeatherIcons.thumbsUp

        ( Motivate, True ) ->
            [ Svg.path [ Svg.Attributes.fill "currentColor", Svg.Attributes.d "M14 9V5a3 3 0 0 0-3-3l-4 9v11h11.28a2 2 0 0 0 2-1.7l1.38-9a2 2 0 0 0-2-2.3zM7 22H4a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2h3" ] []
            ]
                |> FeatherIcons.customIcon

        ( Practice, False ) ->
            FeatherIcons.play

        ( Practice, True ) ->
            [ Svg.polygon [ Svg.Attributes.fill "currentColor", Svg.Attributes.points "5 3 19 12 5 21 5 3" ] []
            ]
                |> FeatherIcons.customIcon

        ( Optimize, False ) ->
            FeatherIcons.user

        ( Optimize, True ) ->
            [ Svg.path [ Svg.Attributes.fill "currentColor", Svg.Attributes.d "M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2" ] []
            , Svg.circle [ Svg.Attributes.fill "currentColor", Svg.Attributes.cx "12", Svg.Attributes.cy "7", Svg.Attributes.r "4" ] []
            ]
                |> FeatherIcons.customIcon
