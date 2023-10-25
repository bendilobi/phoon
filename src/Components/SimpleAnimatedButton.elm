module Components.SimpleAnimatedButton exposing
    ( Model
    , Msg(..)
    , init
    , new
    , update
    , view
    , withDisabled
    , withInline
    , withLightColor
    )

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Swipe as Swipe
import Simple.Transition as Transition



--TODO: Disabled State ordentlich handhaben...
-- SETTINGS


type Button msg
    = Settings
        { model : Model msg
        , label : Element msg
        , toMsg : Msg -> msg
        , isDisabled : Bool
        , isInline : Bool
        , isLightColored : Bool
        }


new :
    { model : Model msg
    , label : Element msg
    , toMsg : Msg -> msg
    }
    -> Button msg
new props =
    Settings
        { model = props.model
        , label = props.label
        , toMsg = props.toMsg
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


type State
    = Default
    | Pressed
    | Disabled


type Model msg
    = Model
        { buttonState : State
        , onPress : Maybe msg
        }


init :
    { onPress : Maybe msg
    }
    -> Model msg
init props =
    Model
        { buttonState = Default
        , onPress = props.onPress
        }



--- Update ---


type Msg
    = ButtonPressed Swipe.Event
    | ButtonReleased Swipe.Event


update :
    { msg : Msg
    , model : Model msg
    , toModel : Model msg -> model
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model msg, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel, effect )
    in
    toParentModel <|
        case props.msg of
            ButtonPressed _ ->
                ( Model
                    { model | buttonState = Pressed }
                , Effect.none
                )

            ButtonReleased _ ->
                --TODO: Kann man irgendwie erkennen, wenn der Finger
                --      beim Release außerhalb des Buttons ist?
                ( Model
                    { model | buttonState = Default }
                , case model.onPress of
                    Nothing ->
                        Effect.none

                    Just msg ->
                        Effect.sendMsg msg
                )



-- VIEW


view : ColorScheme -> Button msg -> Element msg
view colorScheme (Settings settings) =
    let
        (Model model) =
            settings.model

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
                { onPress =
                    if settings.isDisabled then
                        Nothing

                    else
                        model.onPress
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
                                if model.buttonState == Pressed then
                                    CS.interactActiveLighterColor colorScheme

                                else
                                    CS.interactActiveColor colorScheme
                           , htmlAttribute <| Swipe.onStart (\event -> settings.toMsg <| ButtonPressed event)
                           , htmlAttribute <| Swipe.onEnd (\event -> settings.toMsg <| ButtonReleased event)
                           , htmlAttribute <|
                                if model.buttonState == Pressed then
                                    Transition.properties
                                        [ Transition.backgroundColor 100 []
                                        ]

                                else
                                    Transition.properties
                                        [ Transition.backgroundColor 500 [ Transition.easeOutSine ]
                                        ]

                           --TODO: Border color...
                           ]
                    )
                    { onPress =
                        if settings.isDisabled then
                            Nothing

                        else
                            model.onPress
                    , label = settings.label
                    }
