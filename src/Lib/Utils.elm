module Lib.Utils exposing
    ( SessionSound(..)
    , formatSeconds
    )


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



-- TODO: das vielleicht in Session integrieren?


type SessionSound
    = SessionStart
    | Breathing
    | Retention
    | RelaxRetention
    | SessionEnd
