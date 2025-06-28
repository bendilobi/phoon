module Components.EstimateClock exposing (new, view)

import Element exposing (Color, Element, html)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Utils
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


type EstimateClock
    = Settings
        { size : Int
        , zone : Time.Zone
        , now : Time.Posix
        , estimate : Time.Posix
        }


new :
    { size : Int
    , zone : Time.Zone
    , now : Time.Posix
    , estimate : Time.Posix
    }
    -> EstimateClock
new props =
    Settings
        { size = props.size
        , zone = props.zone
        , now = props.now
        , estimate = props.estimate
        }


view : ColorScheme -> EstimateClock -> Element msg
view colorScheme (Settings settings) =
    let
        hour =
            toFloat (Time.toHour settings.zone settings.now)

        minute =
            toFloat (Time.toMinute settings.zone settings.now)

        estimate =
            toFloat (Time.toMinute settings.zone settings.estimate)

        estimateSeconds =
            toFloat (Time.toSecond settings.zone settings.estimate)

        second =
            toFloat (Time.toSecond settings.zone settings.now)

        sizeStr =
            String.fromInt settings.size

        radius =
            settings.size // 2 |> toFloat

        radiusStr =
            settings.size // 2 |> String.fromInt

        handColor =
            CS.primaryPrepareSessionColor colorScheme
    in
    svg
        [ viewBox <| "0 0 " ++ sizeStr ++ " " ++ sizeStr
        , width sizeStr
        , height sizeStr
        ]
        [ circle
            [ cx radiusStr
            , cy radiusStr
            , r radiusStr
            , fill <| Lib.Utils.colorToHex <| CS.primaryMotivationCopyColor colorScheme
            ]
            []
        , viewHand (CS.guideColor colorScheme) 3 (radius / 100 * 77) radius radiusStr ((estimate + (estimateSeconds / 60)) / 60)
        , viewHand handColor 6 (radius / 100 * 52) radius radiusStr ((hour + (minute / 60)) / 12)
        , viewHand handColor 6 (radius / 100 * 77) radius radiusStr ((minute + (second / 60)) / 60)
        ]
        |> html


viewHand : Color -> Int -> Float -> Float -> String -> Float -> Svg.Svg msg
viewHand color width length radius radiusStr turns =
    let
        t =
            2 * pi * (turns - 0.25)

        x =
            radius + length * cos t

        y =
            radius + length * sin t
    in
    line
        [ x1 radiusStr
        , y1 radiusStr
        , x2 (String.fromFloat x)
        , y2 (String.fromFloat y)
        , stroke <| Lib.Utils.colorToHex color
        , strokeWidth (String.fromInt width)
        , strokeLinecap "round"
        ]
        []
