module Components.IntCrementer exposing (new, view, withLightColor, withMax, withMin)

import Components.CrementButton as CrementButton
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Lib.ColorScheme as CS exposing (ColorScheme)
import Simple.Transition as Transition



-- SETTINGS


type IntCrementer msg
    = Settings
        { label : Int -> Element msg
        , onCrement : Int -> msg
        , min : Maybe Int
        , max : Maybe Int
        , isLightColored : Bool
        }


new :
    { label : Int -> Element msg
    , onCrement : Int -> msg
    }
    -> IntCrementer msg
new props =
    Settings
        { label = props.label
        , onCrement = props.onCrement
        , min = Nothing
        , max = Nothing
        , isLightColored = False
        }


withMin : Int -> IntCrementer msg -> IntCrementer msg
withMin min (Settings settings) =
    Settings { settings | min = Just min }


withMax : Int -> IntCrementer msg -> IntCrementer msg
withMax max (Settings settings) =
    Settings { settings | max = Just max }


withLightColor : IntCrementer msg -> IntCrementer msg
withLightColor (Settings settings) =
    Settings { settings | isLightColored = True }


view : ColorScheme -> Int -> IntCrementer msg -> Element msg
view colorScheme currentInt (Settings settings) =
    el [ width fill ] <|
        row
            [ spacing 20
            , Font.size 17
            , centerX
            ]
            [ CrementButton.new
                { onPress = settings.onCrement <| currentInt - 1
                , crement = CrementButton.De
                }
                |> CrementButton.withDisabled (Just currentInt == settings.min)
                |> CrementButton.withLightColor settings.isLightColored
                |> CrementButton.view colorScheme
            , settings.label currentInt
            , CrementButton.new
                { onPress = settings.onCrement <| currentInt + 1
                , crement = CrementButton.In
                }
                |> CrementButton.withDisabled (Just currentInt == settings.max)
                |> CrementButton.withLightColor settings.isLightColored
                |> CrementButton.view colorScheme
            ]
