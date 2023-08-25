module Components.CrementButton exposing
    ( Crement(..)
    , new
    , view
    , withDisabled
      -- , withSize
    )

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import FeatherIcons
import Html.Events as HEvents



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



-- withSize : Int -> Button msg -> Button msg
-- withSize size (Settings settings) =
--     Settings { settings | size = size }
-- VIEW
-- TODO: Anzeige ändern, wenn der Nutzer den Button drückt, ohne loszulassen...
--       Mit Swipe? Oder doch mit :on-hover CSS + ontouchstart... Siehe Internet irgendwo...


view : Button msg -> Element msg
view (Settings settings) =
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
            ([ BG.color <| rgb255 157 154 143
             , Border.color <| rgb255 200 200 200
             , Font.color <| rgb255 200 200 200
             ]
                ++ commonAttributes
            )
        <|
            viewIcon settings.crement settings.size

    else
        el
            ([ BG.color <| rgb255 157 154 143
             , Border.color <| rgb255 200 200 200
             , Font.color <| rgb255 70 70 70
             , Events.onClick settings.onPress
             ]
                ++ commonAttributes
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
