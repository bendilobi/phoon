module Components.IntCrementer exposing (Model, init, new, view, withLightColor, withMax, withMin)

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



--- Model ---


type alias Model =
    CrementButton.Model


init : Model
init =
    CrementButton.init



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
                { onPress =
                    settings.onCrement <|
                        -- if settings.model == CrementButton.Pressed CrementButton.De then
                        --     currentInt - 1
                        -- else
                        --     currentInt
                        case settings.model of
                            --TODO: Das funktioniert nur so halb: beim leave wird crement ausgelöst
                            --      => Brauche ich leave überhaupt? Könnte pointerenter einfach ignorieren
                            CrementButton.Pressed CrementButton.De _ ->
                                currentInt - 1

                            _ ->
                                currentInt
                , crement = CrementButton.De
                , model = settings.model
                }
                |> CrementButton.withDisabled (Just currentInt == settings.min)
                |> CrementButton.withLightColor settings.isLightColored
                |> CrementButton.view colorScheme
            , settings.label currentInt
            , CrementButton.new
                { onPress =
                    settings.onCrement <|
                        -- if settings.model == CrementButton.Pressed CrementButton.In then
                        --     currentInt + 1
                        -- else
                        --     currentInt
                        case settings.model of
                            CrementButton.Pressed CrementButton.In _ ->
                                currentInt + 1

                            _ ->
                                currentInt
                , crement = CrementButton.In
                , model = settings.model
                }
                |> CrementButton.withDisabled (Just currentInt == settings.max)
                |> CrementButton.withLightColor settings.isLightColored
                |> CrementButton.view colorScheme
            ]
