module Components.Dialog exposing
    ( choice
    , new
    , view
    , withWidth
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
        , width : Length

        --TODO: Nonempty List verwenden
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
    column
        [ centerX
        , centerY

        --TODO: Breite aus Bildschirmbreite ableiten
        , width settings.width
        , BG.color <| rgb 1 1 1
        , Border.rounded 15
        , Font.color <| rgb 0 0 0
        ]
        [ column [ width fill, padding 20, spacing 5 ]
            [ paragraph
                [ Font.bold
                , Font.center
                , Font.size 16
                ]
                [ text settings.header ]
            , paragraph
                [ Font.center
                , Font.size 13
                ]
                [ settings.message ]
            ]
        , wrappedRow
            [ width fill

            -- , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
            , Border.color <| rgb 0.6 0.6 0.6
            ]
          <|
            List.map viewChoice settings.choices

        -- [ el
        --     [ width fill
        --     , Border.widthEach { right = 1, top = 0, bottom = 0, left = 0 }
        --     , Border.color <| rgb 0.6 0.6 0.6
        --     ]
        --   <|
        --     el [ centerX, padding 10 ] <|
        --         text "Ok"
        -- , el [ width fill ] <|
        --     el [ centerX, padding 10 ] <|
        --         text "Not Ok"
        -- ]
        ]


viewChoice : Choice msg -> Element msg
viewChoice (ChoiceParams settings) =
    Input.button
        [ width fill
        , Border.widthEach { right = 1, top = 1, bottom = 0, left = 0 }
        , Border.color <| rgb 0.6 0.6 0.6
        ]
        { onPress = Just settings.onChoose
        , label = el [ centerX, padding 10 ] <| text settings.label
        }
