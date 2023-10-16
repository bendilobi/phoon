module Components.BreathingBubble exposing (BreathingBubble)

import Time



--- Settings ---


type BreathingBubble msg
    = Settings
        { model : Model

        -- , toMsg : Msg msg -> msg
        , onFinished : Maybe msg
        }


new :
    { model : Model

    -- , toMsg : Msg msg -> msg
    }
    -> BreathingBubble msg
new props =
    Settings
        { model = props.model

        -- , toMsg = props.toMsg
        , onFinished = Nothing
        }



--- Modifiers ---


withOnFinished : msg -> BreathingBubble msg -> BreathingBubble msg
withOnFinished onFinished (Settings settings) =
    Settings { settings | onFinished = Just onFinished }



--- Model ---


type BreathType
    = In
    | Out


type Model
    = Starting
    | AtBreath Int BreathType
    | BreathingFinished


init : Model
init =
    Starting



--- Update ---


type Msg
    = Tick Time.Posix



--- View ---
