module Components.Dialog exposing
    ( choice
    , new
    , view
    )

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Lib.ColorScheme as CS exposing (ColorScheme)



-- SETTINGS


type Dialog msg
    = Settings
        { header : String
        , message : Element msg
        , screenWidth : Float
        , choices : List (Choice msg)
        }


type Choice msg
    = ChoiceParams
        { label : String
        , onChoose : msg
        }



-- CREATION


new :
    { header : String
    , message : Element msg
    , choices : List (Choice msg)
    , screenWidth : Float
    }
    -> Dialog msg
new props =
    Settings
        { header = props.header
        , message = props.message
        , choices = props.choices
        , screenWidth = props.screenWidth
        }


choice :
    { label : String
    , onChoose : msg
    }
    -> Choice msg
choice props =
    ChoiceParams
        { label = props.label
        , onChoose = props.onChoose
        }



-- MODIFIERS
-- withWidth : Length -> Dialog msg -> Dialog msg
-- withWidth length (Settings settings) =
--     Settings { settings | screenWidth = length }
-- VIEW


view : ColorScheme -> Dialog msg -> Element msg
view colorScheme (Settings settings) =
    let
        borderColor =
            rgb 0.8 0.8 0.8
    in
    column
        [ centerX
        , centerY
        , width (shrink |> maximum (settings.screenWidth * 0.8 |> round))
        , BG.color <| rgb 1 1 1
        , Border.rounded 15
        , clip
        , Font.color <| rgb 0 0 0
        ]
        [ column
            [ width fill
            , spacing 10
            , paddingXY 20 25
            ]
            [ paragraph
                [ Font.bold
                , Font.center
                , Font.size 18
                ]
                [ text settings.header ]
            , paragraph
                [ Font.center
                , Font.size 14
                ]
                [ settings.message ]
            ]
        , el [ width fill, height <| px 1, BG.color borderColor ] <| none
        , wrappedRow
            [ width fill
            , BG.color borderColor
            , spacing 1
            , Font.color <| CS.interactActiveLighterColor colorScheme
            ]
          <|
            List.map viewChoice settings.choices
        ]


viewChoice : Choice msg -> Element msg
viewChoice (ChoiceParams settings) =
    Input.button
        [ width fill
        ]
        { onPress = Just settings.onChoose
        , label =
            el
                [ width fill
                , Font.center
                , Font.size 17
                , BG.color <| rgb 1 1 1

                --TODO: For some strange reason, this doesn't consistently work
                --      on iOS (Android not tested). Sometimes the color changes,
                --      sometimes it doesn't. Sometimes only on long press...
                , mouseOver [ BG.color <| rgb 0.8 0.8 0.8 ]
                , paddingXY 5 15
                ]
            <|
                text settings.label
        }
