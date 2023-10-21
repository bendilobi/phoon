module Components.CrementButton exposing
    ( Crement(..)
    , new
    , view
    , withDisabled
    , withLightColor
    )

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
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
        , isLightColored : Bool
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
        , isLightColored = False
        }



-- MODIFIERS


withDisabled : Bool -> Button msg -> Button msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }


withLightColor : Bool -> Button msg -> Button msg
withLightColor light (Settings settings) =
    Settings { settings | isLightColored = light }



-- VIEW


view : ColorScheme -> Button msg -> Element msg
view colorScheme (Settings settings) =
    let
        size =
            50

        iconSize =
            35

        commonAttributes =
            [ width <| px size
            , height <| px size
            , Border.rounded <| size // 2
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
            el [ centerX, centerY ] <|
                viewIcon settings.crement iconSize

    else
        button
            ((if settings.isLightColored then
                CS.interactActiveLighter colorScheme

              else
                CS.interactActive colorScheme
             )
                ++ commonAttributes
            )
            { onPress = Just settings.onPress
            , label = el [ centerX, centerY ] <| viewIcon settings.crement iconSize
            }


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
