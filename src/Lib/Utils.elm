module Lib.Utils exposing
    ( SessionSound(..)
    , formatSeconds
    , viewRetentionTimes
    )

-- TODO: Brauche ich das für die Anzeige der Ende-Uhrzeit?
-- formatTime : Int -> String
-- formatTime millis =
--     Time.millisToPosix millis
--         --(millis + offset)
--         |> Clock.fromPosix
--         |> (\time ->
--                 String.fromInt (Clock.getHours time)
--                     ++ ":"
--                     ++ String.padLeft 2 '0' (String.fromInt (Clock.getMinutes time))
--            )

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font


formatSeconds : Int -> String
formatSeconds sec =
    let
        pad =
            String.padLeft 2 '0'

        -- hours =
        --     sec // 60 // 60
        minutes =
            remainderBy 60 (sec // 60)

        seconds =
            remainderBy 60 sec

        pos =
            if minutes > 0 then
                [ String.fromInt minutes
                , pad <| String.fromInt seconds
                ]

            else
                [ String.fromInt seconds ]
    in
    String.join ":" pos


type SessionSound
    = Breathing
    | Retention
    | RelaxRetention
    | SessionEnd



-- TODO: Das als eine "Component" umsetzen?


viewRetentionTimes : List Int -> Element msg
viewRetentionTimes times =
    let
        meanTime =
            List.sum times // List.length times
    in
    column
        [ spacing 10
        , centerX
        , centerY
        , Font.alignRight
        ]
    <|
        List.map2
            (\i t ->
                row [ width fill ]
                    [ el [ width fill ] <| text <| "Runde " ++ String.fromInt i ++ ": "
                    , el [ Font.bold ] <| text <| formatRetentionTime t
                    ]
            )
            (List.range 1 (List.length times))
            times
            ++ [ row
                    [ width fill
                    , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                    , paddingXY 0 7
                    ]
                    [ el [] <| text "Durchschnitt: "
                    , el
                        [ Font.bold
                        ]
                      <|
                        text <|
                            formatRetentionTime meanTime
                    ]
               ]


formatRetentionTime : Int -> String
formatRetentionTime seconds =
    String.join ":"
        [ String.padLeft 1 '0' <| String.fromInt <| remainderBy 60 (seconds // 60)
        , String.padLeft 2 '0' <| String.fromInt <| remainderBy 60 seconds
        ]
