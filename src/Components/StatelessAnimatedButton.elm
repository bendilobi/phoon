module Components.StatelessAnimatedButton exposing (Model(..), init, new, view, withDisabled, withInline, withLightColor)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Html.Events as HEvents
import Json.Decode as Decode
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Swipe as Swipe
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



--- Model ---


type Model
    = Pressed
    | Released


init : Model
init =
    Released



-- VIEW


view : ColorScheme -> Button msg -> Element msg
view colorScheme (Settings settings) =
    let
        commonAttributes =
            [ width fill
            , padding 20
            , Font.center
            , Border.rounded 15
            , Border.width 1
            ]

        animationAttributes =
            [ htmlAttribute <|
                case settings.model of
                    Pressed ->
                        Transition.properties
                            [ Transition.backgroundColor 50 []
                            ]

                    Released ->
                        Transition.properties
                            [ Transition.backgroundColor 1300 [ Transition.easeOutQuad ]
                            ]
            ]

        eventAttributes =
            -- [ htmlAttribute <| Swipe.onStart (\_ -> settings.onPress Pressed)
            [ htmlAttribute <| HEvents.on "pointerdown" <| Decode.succeed <| settings.onPress Pressed

            -- , htmlAttribute <|
            --     Swipe.onEndWithOptions
            --         { --- This is needed, otherwise the button remains in Pressed state sometimes
            --           stopPropagation = True
            --         --- This is needed, otherwise strange behavior such as button being
            --         --- triggered again after touch end...
            --         , preventDefault = False
            --         }
            --         (\_ -> settings.onPress Released)
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
                        ([ BG.color <|
                            case settings.model of
                                Pressed ->
                                    --TODO: Ins Farbschema aufnehmen?
                                    rgba 0.8 0.8 0.8 1.0

                                Released ->
                                    rgba 0.8 0.8 0.8 0
                         , Border.rounded 5
                         , padding 5
                         , moveUp 5
                         , moveLeft 5
                         ]
                            ++ animationAttributes
                        )
                    <|
                        --- Make the shape adapt to the button's size
                        el [ transparent True ]
                        <|
                            settings.label
                 ]
                    ++ eventAttributes
                )
                -- { onPress = Just <| settings.onPress settings.model
                { onPress = Just <| settings.onPress Released
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
                        ++ (if settings.isLightColored then
                                CS.interactActiveLighter colorScheme

                            else
                                CS.interactActive colorScheme
                           )
                        ++ [ BG.color <|
                                case settings.model of
                                    Pressed ->
                                        CS.interactActiveLighterColor colorScheme

                                    Released ->
                                        CS.interactActiveColor colorScheme
                           ]
                        ++ animationAttributes
                        ++ eventAttributes
                    )
                    -- { onPress = Just <| settings.onPress settings.model
                    { onPress = Just <| settings.onPress Released
                    , label = settings.label
                    }
