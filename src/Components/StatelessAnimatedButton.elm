module Components.StatelessAnimatedButton exposing (Model(..), init, new, view, withAnimated, withDisabled, withInline, withLightColor)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html.Events as HEvents
import Json.Decode as Decode
import Lib.ColorScheme as CS exposing (ColorScheme)
import Simple.Transition as Transition



-- SETTINGS


type Button msg
    = Settings
        { label : Element msg
        , onPress : Model -> msg
        , model : Model
        , isDisabled : Bool
        , isInline : Bool
        , isLightColored : Bool
        , isAnimated : Bool
        }


new :
    { onPress : Model -> msg
    , label : Element msg
    , model : Model
    }
    -> Button msg
new props =
    Settings
        { label = props.label
        , onPress = props.onPress
        , model = props.model
        , isDisabled = False
        , isInline = False
        , isLightColored = False
        , isAnimated = True
        }



-- MODIFIERS


withDisabled : Bool -> Button msg -> Button msg
withDisabled isDisabled (Settings settings) =
    Settings { settings | isDisabled = isDisabled }


withInline : Button msg -> Button msg
withInline (Settings settings) =
    Settings { settings | isInline = True }


withLightColor : Button msg -> Button msg
withLightColor (Settings settings) =
    Settings { settings | isLightColored = True }


withAnimated : Bool -> Button msg -> Button msg
withAnimated animated (Settings settings) =
    Settings { settings | isAnimated = animated }



--- Model ---


type Model
    = Pressed Bool
    | Released
    | Cancelled


init : Model
init =
    Released



-- reset : Model
-- reset =
--     Released
-- VIEW


view : ColorScheme -> Button msg -> Element msg
view colorScheme (Settings settings) =
    let
        commonAttributes =
            [ width fill
            , padding 20
            , Font.center
            , Border.rounded 15

            -- , Border.width 1
            -- , Border.shadow
            --     { offset = ( 3, 3 )
            --     , size = 0
            --     , blur = 10
            --     , color = rgb 0.5 0.5 0.5
            --     }
            ]

        animationAttributes =
            case settings.model of
                Pressed _ ->
                    [ htmlAttribute <|
                        Transition.properties
                            [ Transition.backgroundColor 50 []
                            ]
                    ]

                Released ->
                    [ htmlAttribute <|
                        Transition.properties
                            [ Transition.backgroundColor 1300 [ Transition.easeOutQuad ]
                            ]
                    ]

                Cancelled ->
                    []

        eventAttributes =
            --TODO: Erklären, warum ich hier was mache...
            [ htmlAttribute <| HEvents.on "pointerdown" <| Decode.succeed <| settings.onPress <| Pressed True
            , htmlAttribute <| HEvents.on "pointerenter" <| Decode.succeed <| settings.onPress <| Pressed False
            , htmlAttribute <| HEvents.on "pointerup" <| Decode.succeed <| settings.onPress Released
            , htmlAttribute <| HEvents.on "pointercancel" <| Decode.succeed <| settings.onPress Cancelled
            , htmlAttribute <|
                HEvents.on "pointerleave" <|
                    Decode.succeed <|
                        settings.onPress <|
                            case settings.model of
                                Pressed False ->
                                    Cancelled

                                _ ->
                                    Released

            --TODO: Das hier funktioniert nicht, weil pointerleave nach einem pointerdown nicht mehr
            --      registriert wird (pointercapture, siehe https://developer.mozilla.org/en-US/docs/Web/API/Element/pointerdown_event)
            -- , htmlAttribute <| HEvents.on "pointerleave" <| Decode.succeed <| settings.onPress Cancelled
            -- , htmlAttribute <| HEvents.on "pointerout" <| Decode.succeed <| settings.onPress Cancelled
            ]
    in
    if settings.isInline then
        if settings.isDisabled then
            el [ Font.color <| CS.interactInactiveDarkerColor colorScheme ] settings.label

        else
            button
                ([ Font.color <|
                    if settings.isLightColored then
                        CS.interactActiveLighterColor colorScheme

                    else
                        CS.interactActiveColor colorScheme
                 , behindContent <|
                    --- To give it a bit of padding without affecting the layout
                    el
                        ([ Border.rounded 5
                         , padding 5
                         , moveUp 5
                         , moveLeft 5
                         ]
                            ++ (if settings.isAnimated then
                                    (BG.color <|
                                        case settings.model of
                                            Pressed _ ->
                                                --TODO: Ins Farbschema aufnehmen?
                                                rgba 0.8 0.8 0.8 1.0

                                            _ ->
                                                rgba 0.8 0.8 0.8 0
                                    )
                                        :: animationAttributes

                                else
                                    []
                               )
                        )
                    <|
                        --- Make the shape adapt to the button's size
                        el [ transparent True ]
                        <|
                            settings.label
                 ]
                    ++ eventAttributes
                )
                --TODO: Input.button nicht mehr verwenden, sondern direkt rendern
                { onPress = Nothing --Just <| settings.onPress Released
                , label = settings.label
                }

    else if settings.isDisabled then
        el
            (commonAttributes ++ CS.interactInactive colorScheme)
        <|
            settings.label

    else
        --TODO: Anscheinend kann das Problem mit den falsch getriggerten Animationen
        --      gelöst werden, wenn der Button in zwei (!) els eingepackt wird...
        --      Wirklich? Warum? Im Elm-ui Slack zur Sprache bringen?
        el [ width fill ] <|
            el [ width fill ] <|
                button
                    (commonAttributes
                        --TODO: Animationsfarbe so definieren, dass lightColor wieder funktioniert
                        ++ (if settings.isLightColored then
                                CS.interactActiveLighter colorScheme

                            else
                                CS.interactActive colorScheme
                           )
                        ++ (if settings.isAnimated then
                                (BG.color <|
                                    case settings.model of
                                        Pressed _ ->
                                            CS.interactActiveLighterColor colorScheme

                                        _ ->
                                            CS.interactActiveColor colorScheme
                                )
                                    :: animationAttributes

                            else
                                []
                           )
                        ++ eventAttributes
                    )
                    { onPress = Nothing --Just <| settings.onPress Released

                    -- , label = settings.label
                    , label =
                        text <|
                            case settings.model of
                                Pressed True ->
                                    "Pressed & Captured"

                                Pressed False ->
                                    "Pressed & Not Cap."

                                Released ->
                                    "Released"

                                Cancelled ->
                                    "Cancelled"
                    }
