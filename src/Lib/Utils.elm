module Lib.Utils exposing
    ( SessionSound(..)
    , formatSeconds
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
