module Components.CrementButton exposing
    ( Crement(..)
    , Model(..)
    , init
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
import Element.Input as Input
import FeatherIcons
import Html.Events as HEvents
import Json.Decode as Decode
import Lib.ColorScheme as CS exposing (ColorScheme)
import Simple.Transition as Transition



-- SETTINGS


type Crement
    = In
    | De


type Button msg
    = Settings
        { crement : Crement
        , number : Int
        , stepSize : Int
        , onPress : Int -> Model -> msg
        , model : Model
        , isDisabled : Bool
        , isLightColored : Bool
        }


new :
    { onPress : Int -> Model -> msg
    , crement : Crement
    , number : Int
    , stepSize : Int
    , model : Model
    }
    -> Button msg
new props =
    Settings
        { crement = props.crement
        , number = props.number
        , stepSize = props.stepSize
        , onPress = props.onPress
        , model = props.model
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



--- Model ---


type Model
    = Pressed Crement Bool
    | Released
    | Triggered
    | Cancelled


init : Model
init =
    Released



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
            , centerX
            , centerY
            , padding 0
            ]

        crementedNumber =
            case settings.crement of
                In ->
                    settings.number + settings.stepSize

                De ->
                    settings.number - settings.stepSize
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
        el
            (commonAttributes
                ++ (let
                        releasedColors =
                            if settings.isLightColored then
                                CS.interactActiveLighter colorScheme

                            else
                                CS.interactActive colorScheme
                    in
                    case settings.model of
                        Pressed crement _ ->
                            if crement == settings.crement then
                                --- It's-a-me!
                                if settings.isLightColored then
                                    CS.interactActive colorScheme

                                else
                                    CS.interactActiveLighter colorScheme

                            else
                                releasedColors

                        _ ->
                            releasedColors
                   )
                ++ [ pointer
                   , htmlAttribute <|
                        HEvents.on "pointerdown" <|
                            Decode.succeed <|
                                settings.onPress settings.number <|
                                    Pressed settings.crement True
                   , htmlAttribute <|
                        HEvents.on "pointerenter" <|
                            Decode.succeed <|
                                settings.onPress settings.number <|
                                    Pressed settings.crement False
                   , htmlAttribute <|
                        HEvents.on "pointercancel" <|
                            Decode.succeed <|
                                settings.onPress settings.number Cancelled
                   , htmlAttribute <|
                        HEvents.on "pointerleave" <|
                            Decode.succeed <|
                                case settings.model of
                                    Pressed _ False ->
                                        settings.onPress settings.number Cancelled

                                    Pressed _ True ->
                                        settings.onPress settings.number Released

                                    Cancelled ->
                                        settings.onPress settings.number Cancelled

                                    Released ->
                                        settings.onPress settings.number Released

                                    Triggered ->
                                        settings.onPress settings.number Cancelled
                   , htmlAttribute <|
                        HEvents.on "pointerup" <|
                            Decode.succeed <|
                                settings.onPress crementedNumber Triggered
                   ]
                ++ (case settings.model of
                        Pressed _ _ ->
                            []

                        -- [ htmlAttribute <|
                        --     Transition.properties
                        --         [ Transition.backgroundColor 200 []
                        --         ]
                        -- ]
                        Cancelled ->
                            []

                        _ ->
                            [ htmlAttribute <|
                                Transition.properties
                                    [ Transition.backgroundColor 1000 [ Transition.easeOutQuad ]
                                    ]
                            ]
                   )
            )
        <|
            el [ centerX, centerY ] <|
                viewIcon settings.crement iconSize


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
