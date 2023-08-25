module Components.Button exposing (new, view, withDisabled)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)



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


view : Button msg -> Element msg
view (Settings settings) =
    let
        commonAttributes =
            [ width fill
            , padding 20
            , Font.color <| rgb 1 1 1
            , Font.center
            , Border.rounded 15
            , Border.width 1
            ]
    in
    if settings.isDisabled then
        el
            ([ BG.color <| rgb255 157 154 143
             , Border.color <| rgb255 72 70 66
             ]
                ++ commonAttributes
            )
        <|
            settings.label

    else
        button
            ([ BG.color <| rgb255 33 33 33
             , Border.color <| rgb 0 0 0
             ]
                ++ commonAttributes
            )
            { onPress =
                if settings.isDisabled then
                    Nothing

                else
                    settings.onPress
            , label = settings.label
            }
