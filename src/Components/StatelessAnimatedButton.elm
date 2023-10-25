module Components.StatelessAnimatedButton exposing (Model(..), init, new, view, withDisabled, withInline, withLightColor)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Swipe as Swipe
import Simple.Transition as Transition



-- SETTINGS


type Model
    = Pressed
    | Default



-- type alias State =
--     Bool


init : Model
init =
    Default


type Button msg
    = Settings
        { label : Element msg

        -- , onPress : Maybe msg
        , onPress : Model -> msg
        , model : Model
        , isDisabled : Bool
        , isInline : Bool
        , isLightColored : Bool
        }


new :
    -- { onPress : Maybe msg
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
    in
    if settings.isInline then
        if settings.isDisabled then
            el [ Font.color <| CS.interactInactiveDarkerColor colorScheme ] settings.label

        else
            button
                [ Font.color <|
                    if settings.isLightColored then
                        CS.interactActiveLighterColor colorScheme

                    else
                        CS.interactActiveColor colorScheme
                ]
                { onPress = Just <| settings.onPress settings.model

                -- if settings.isDisabled then
                --     Nothing
                -- else
                --     settings.onPress
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

                                    Default ->
                                        CS.interactActiveColor colorScheme
                           , htmlAttribute <| Swipe.onStart (\_ -> settings.onPress Pressed)
                           , htmlAttribute <| Swipe.onEnd (\_ -> settings.onPress Default)
                           , htmlAttribute <|
                                case settings.model of
                                    Pressed ->
                                        Transition.properties
                                            [ Transition.backgroundColor 50 []
                                            ]

                                    Default ->
                                        Transition.properties
                                            [ Transition.backgroundColor 1300 [ Transition.easeOutQuad ]
                                            ]
                           ]
                    )
                    { onPress = Just <| settings.onPress settings.model

                    -- if settings.isDisabled then
                    --     Nothing
                    -- else
                    --     settings.onPress
                    , label = settings.label
                    }
