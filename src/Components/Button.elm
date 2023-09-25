module Components.Button exposing (new, view, withDisabled)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Lib.ColorScheme as CS exposing (ColorScheme)



-- SETTINGS


type Button msg
    = Settings
        { label : Element msg
        , onPress : Maybe msg
        , isDisabled : Bool
        }


new :
    { onPress : Maybe msg
    , label : Element msg
    }
    -> Button msg
new props =
    Settings
        { label = props.label
        , onPress = props.onPress
        , isDisabled = False
        }



-- MODIFIERS


withDisabled : Bool -> Button msg -> Button msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }



-- VIEW
-- TODO: Anzeige ändern, wenn der Nutzer den Button drückt, ohne loszulassen...
--       Mit Swipe? Oder doch mit :on-hover CSS + ontouchstart... Siehe Internet irgendwo...


view : ColorScheme -> Button msg -> Element msg
view colorScheme (Settings settings) =
    let
        commonAttributes =
            [ width fill
            , padding 20

            -- , Font.color <| rgb255 241 241 230
            , Font.center
            , Border.rounded 15
            , Border.width 1
            ]
    in
    if settings.isDisabled then
        el
            (commonAttributes ++ CS.interactInactive colorScheme)
        <|
            settings.label

    else
        button
            (commonAttributes ++ CS.interactActive colorScheme)
            { onPress =
                if settings.isDisabled then
                    Nothing

                else
                    settings.onPress
            , label = settings.label
            }
