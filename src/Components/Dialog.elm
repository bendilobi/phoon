module Components.Dialog exposing
    ( choice
    , new
    , view
    , withWidth
    )

import Chart.Attributes exposing (border)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Lib.ColorScheme as CS exposing (ColorScheme)
import Simple.Transition exposing (borderColor)



-- SETTINGS


type Dialog msg
    = Settings
        { header : String
        , message : Element msg
        , width : Length
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
    }
    -> Dialog msg
new props =
    Settings
        { header = props.header
        , message = props.message
        , choices = props.choices
        , width = px 300
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


withWidth : Length -> Dialog msg -> Dialog msg
withWidth length (Settings settings) =
    Settings { settings | width = length }



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
        , width settings.width
        , BG.color <| rgb 1 1 1
        , Border.rounded 15
        , clip
        , Font.color <| rgb 0 0 0
        ]
        [ column
            [ width fill

            -- , paddingEach { top = 20, bottom = 30, left = 20, right = 20 }
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
        , BG.color <|
            rgb 1 1 1

        --TODO: anscheinend ist da die :active Pseudoclass dahinter. Funktioniert
        --      mit Touch wohl nur mit einem Trick:
        --      https://blog.tomoyukikashiro.me/post/how-to-set-active-style-to-button-in-mobile
        --      https://stackoverflow.com/questions/8330559/hover-effects-using-css3-touch-events
        --      https://dev.webonomic.nl/fixing-the-iphone-css-hover-problem-on-ios
        --      https://www.sitepoint.com/5-ways-prevent-300ms-click-delay-mobile-devices/
        --      aber wie baue ich das ein?
        , mouseDown [ BG.color <| rgb 0.8 0.8 0.8 ]
        , mouseOver [ BG.color <| rgb 0.8 0.8 0.8 ]

        -- , htmlAttribute <| Html.Attributes.style "-webkit-tap-highlight-color" "rgba(0,0,0,0)"
        ]
        { onPress = Just settings.onChoose
        , label = el [ centerX, paddingXY 5 15 ] <| text settings.label
        }
