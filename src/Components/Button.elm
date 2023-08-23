module Components.Button exposing (view)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)


view :
    { onPress : Maybe msg
    , label : Element msg
    }
    -> Element msg
view props =
    button
        [ padding 20
        , BG.color <| rgb255 33 33 33
        , Font.color <| rgb 1 1 1
        , Border.rounded 15
        , Border.width 1
        , Border.color <| rgb 0 0 0
        ]
        props
