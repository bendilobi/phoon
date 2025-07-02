module Components.EstimateClock exposing (new, view)

import Element exposing (Color, Element, html)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Utils
import String.Format
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


type EstimateClock
    = Settings
        { size : Int
        , zone : Time.Zone
        , now : Time.Posix
        , estimate : Time.Posix
        , cycleEstimates : List Time.Posix
        }


new :
    { size : Int
    , zone : Time.Zone
    , now : Time.Posix
    , estimate : Time.Posix
    , cycleEstimates : List Time.Posix
    }
    -> EstimateClock
new props =
    Settings
        { size = props.size
        , zone = props.zone
        , now = props.now
        , estimate = props.estimate
        , cycleEstimates = props.cycleEstimates
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

        cycleEstimates =
            settings.cycleEstimates
                |> List.map (Time.toMinute settings.zone)
                |> List.map toFloat

        cycleEstimatesSeconds =
            settings.cycleEstimates
                |> List.map (Time.toSecond settings.zone)
                |> List.map toFloat

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
        ([ circle
            [ cx radiusStr
            , cy radiusStr
            , r radiusStr
            , fill <| Lib.Utils.colorToHex <| CS.primaryMotivationCopyColor colorScheme
            ]
            []
         , viewQuarterLine handColor radius 0.25
         , viewQuarterLine handColor radius 0.5
         , viewQuarterLine handColor radius 0.75
         , viewQuarterLine handColor radius 1
         , viewEstimate
            (CS.guideColor colorScheme)
            (radius / 100 * 77)
            radius
            radiusStr
            ((minute + (second / 60)) / 60)
            ((estimate + (estimateSeconds / 60)) / 60)
            {- True if the session takes longer than 30 minutes. Needed because the pie segment has to
               be drawn differently if it is larger than 180 degrees.
            -}
            (Time.posixToMillis settings.estimate - Time.posixToMillis settings.now > 1800000)
         ]
            ++ List.map2
                (\e es ->
                    viewHand (CS.primaryMotivationCopyColor colorScheme)
                        1
                        (radius / 100 * 80)
                        radius
                        radiusStr
                        ((e + (es / 60)) / 60)
                )
                cycleEstimates
                cycleEstimatesSeconds
            ++ [ viewHand (CS.guideLightColor colorScheme)
                    6
                    (radius / 100 * 77)
                    radius
                    radiusStr
                    ((estimate + (estimateSeconds / 60)) / 60)
               , viewHand handColor 6 (radius / 100 * 52) radius radiusStr ((hour + (minute / 60)) / 12)
               , viewHand handColor 6 (radius / 100 * 77) radius radiusStr ((minute + (second / 60)) / 60)
               ]
        )
        |> html


viewQuarterLine : Color -> Float -> Float -> Svg.Svg msg
viewQuarterLine color radius turns =
    let
        t =
            2 * pi * turns

        x_1 =
            radius + (radius / 100 * 85) * cos t |> String.fromFloat

        y_1 =
            radius + (radius / 100 * 85) * sin t |> String.fromFloat

        x_2 =
            radius + (radius / 100 * 95) * cos t |> String.fromFloat

        y_2 =
            radius + (radius / 100 * 95) * sin t |> String.fromFloat
    in
    line
        [ x1 x_1
        , y1 y_1
        , x2 x_2
        , y2 y_2
        , stroke <| Lib.Utils.colorToHex color
        , strokeWidth "2"
        , strokeLinecap "round"
        ]
        []


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


viewEstimate : Color -> Float -> Float -> String -> Float -> Float -> Bool -> Svg.Svg msg
viewEstimate color length radius radiusStr thetaStart thetaEnd largeArc =
    let
        tStart =
            2 * pi * (thetaStart - 0.25)

        tEnd =
            2 * pi * (thetaEnd - 0.25)
    in
    Svg.path
        [ d
            ("M {{x1}} {{y1}} L {{x2}} {{y2}} A {{radius}} {{radius}} 0 {{largeArc}} 1 {{x3}} {{y3}} Z"
                |> String.Format.namedValue "x1" radiusStr
                |> String.Format.namedValue "y1" radiusStr
                |> String.Format.namedValue "x2" (radius + length * cos tStart |> String.fromFloat)
                |> String.Format.namedValue "y2" (radius + length * sin tStart |> String.fromFloat)
                |> String.Format.namedValue "radius" (String.fromFloat length)
                |> String.Format.namedValue "largeArc"
                    (if largeArc then
                        "1"

                     else
                        "0"
                    )
                |> String.Format.namedValue "x3" (radius + length * cos tEnd |> String.fromFloat)
                |> String.Format.namedValue "y3" (radius + length * sin tEnd |> String.fromFloat)
            )
        , stroke <| Lib.Utils.colorToHex color
        , strokeWidth "0" --<| String.fromFloat length
        , fill <| Lib.Utils.colorToHex color
        , fillOpacity "0.35"

        -- , strokeLinecap "round"
        ]
        []
