module Components.RadioGroup exposing (Layout, new, view, withLayout, withLightColor, withSelected)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Lib.ColorScheme as CS exposing (ColorScheme)



--TODO: abchecken, ob ich lieber das von elm-ui verwenden sollte:
--https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Input#radio


type Layout
    = Horizontal
    | Vertical


type RadioGroup choicesType msg
    = Settings
        { layout : Layout
        , choices : List choicesType
        , onSelect : choicesType -> msg
        , toString : choicesType -> String
        , selected : Maybe choicesType
        , isLightColored : Bool
        }


new :
    { choices : List choicesType
    , toString : choicesType -> String
    , onSelect : choicesType -> msg

    -- , selected : choicesType
    }
    -> RadioGroup choicesType msg
new { choices, toString, onSelect } =
    Settings
        --TODO: layout = Vertical implementieren? Oder weg?
        { layout = Horizontal
        , choices = choices
        , toString = toString
        , onSelect = onSelect
        , selected = Nothing
        , isLightColored = False
        }


withLayout : Layout -> RadioGroup choicesType msg -> RadioGroup choicesType msg
withLayout layout (Settings settings) =
    Settings
        { settings | layout = layout }


withSelected : choicesType -> RadioGroup choicesType msg -> RadioGroup choicesType msg
withSelected choice (Settings settings) =
    Settings
        { settings | selected = Just choice }


withLightColor : Bool -> RadioGroup choicesType msg -> RadioGroup choicesType msg
withLightColor light (Settings settings) =
    Settings { settings | isLightColored = light }


view : ColorScheme -> RadioGroup choicesType msg -> Element msg
view colorScheme (Settings settings) =
    let
        separator =
            el [] <| text "|"

        viewItem item =
            if Just item == settings.selected then
                -- el [ Font.color <| CS.interactInactiveDarkerColor colorScheme ] <|
                el
                    [ Font.color <| CS.guideColor colorScheme ]
                <|
                    text <|
                        settings.toString item

            else
                Input.button
                    [ Font.color <|
                        if settings.isLightColored then
                            CS.interactActiveLighterColor colorScheme

                        else
                            CS.interactActiveColor colorScheme
                    ]
                    { onPress = Just <| settings.onSelect item
                    , label = text <| settings.toString item
                    }
    in
    el [ width fill ] <|
        row
            [ centerX
            , spacing 15
            , Font.size 20
            , Font.bold
            ]
        <|
            List.intersperse separator <|
                List.map viewItem settings.choices
