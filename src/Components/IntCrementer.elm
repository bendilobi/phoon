module Components.IntCrementer exposing (Model, init, new, view, wasTriggered, withLightColor, withMax, withMin, withStepSize)

import Components.CrementButton as CrementButton
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Lib.ColorScheme as CS exposing (ColorScheme)



-- SETTINGS


type IntCrementer msg
    = Settings
        { label : Int -> Element msg
        , onCrement : Int -> CrementButton.Model -> msg
        , model : CrementButton.Model
        , min : Maybe Int
        , max : Maybe Int
        , stepSize : Int
        , isLightColored : Bool
        }


new :
    { label : Int -> Element msg
    , onCrement : Int -> CrementButton.Model -> msg
    , model : CrementButton.Model
    }
    -> IntCrementer msg
new props =
    Settings
        { label = props.label
        , onCrement = props.onCrement
        , model = props.model
        , min = Nothing
        , max = Nothing
        , stepSize = 1
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


withStepSize : Int -> IntCrementer msg -> IntCrementer msg
withStepSize size (Settings settings) =
    Settings { settings | stepSize = size }



--- Model ---


type alias Model =
    CrementButton.Model


init : Model
init =
    CrementButton.init


wasTriggered : CrementButton.Model -> Bool
wasTriggered buttonState =
    buttonState == CrementButton.Triggered



--- View ---


view : ColorScheme -> Int -> IntCrementer msg -> Element msg
view colorScheme currentInt (Settings settings) =
    el [ width fill ] <|
        row
            [ spacing 20
            , Font.size 17
            , centerX
            ]
            [ CrementButton.new
                { onPress = settings.onCrement
                , number = currentInt
                , stepSize = settings.stepSize
                , crement = CrementButton.De
                , model = settings.model
                }
                |> CrementButton.withDisabled (Just currentInt == settings.min)
                |> CrementButton.withLightColor settings.isLightColored
                |> CrementButton.view colorScheme
            , el [ width fill, Font.center ] <| settings.label currentInt
            , CrementButton.new
                { onPress = settings.onCrement
                , number = currentInt
                , stepSize = settings.stepSize
                , crement = CrementButton.In
                , model = settings.model
                }
                |> CrementButton.withDisabled (Just currentInt == settings.max)
                |> CrementButton.withLightColor settings.isLightColored
                |> CrementButton.view colorScheme
            ]
