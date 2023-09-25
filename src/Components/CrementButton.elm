module Components.CrementButton exposing
    ( Crement(..)
    , new
    , view
    , withDisabled
    )

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Lib.ColorScheme as CS exposing (ColorScheme)



-- SETTINGS


type Crement
    = In
    | De


type Button msg
    = Settings
        { crement : Crement
        , onPress : msg
        , isDisabled : Bool
        , size : Int
        }


new :
    { onPress : msg
    , crement : Crement
    }
    -> Button msg
new props =
    Settings
        { crement = props.crement
        , onPress = props.onPress
        , isDisabled = False
        , size = 50
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
            [ width <| px settings.size
            , height <| px settings.size

            -- , Font.color <| rgb 1 1 1
            , Border.rounded <| settings.size // 2
            , Border.width 1
            , centerX
            , centerY
            , padding 0
            ]
    in
    if settings.isDisabled then
        el
            (CS.interactInactive colorScheme
                ++ commonAttributes
            )
        <|
            viewIcon settings.crement settings.size

    else
        el
            ([ Events.onClick settings.onPress
             ]
                ++ commonAttributes
                ++ CS.interactActive colorScheme
            )
        <|
            viewIcon settings.crement settings.size


viewIcon : Crement -> Int -> Element msg
viewIcon crement size =
    let
        icon =
            if crement == In then
                FeatherIcons.plus

            else
                FeatherIcons.minus
    in
    icon
        |> FeatherIcons.withSize (toFloat size)
        |> FeatherIcons.toHtml []
        |> html
