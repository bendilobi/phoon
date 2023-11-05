module Components.AnimatorButton exposing
    ( Model
    , Msg(..)
    , animator
    , init
    , new
    , update
    , view
    , withDisabled
    , withInline
    , withLightColor
    )

import Animator
import Color
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Swipe as Swipe
import Time



--TODO: Disabled State ordentlich handhaben...
-- SETTINGS


type Button msg
    = Settings
        { model : Model msg
        , label : Element msg

        -- , onPress : Maybe msg
        , toMsg : Msg -> msg
        , isDisabled : Bool
        , isInline : Bool
        , isLightColored : Bool
        }


new :
    { model : Model msg

    -- , onPress : Maybe msg
    , label : Element msg
    , toMsg : Msg -> msg
    }
    -> Button msg
new props =
    Settings
        { model = props.model
        , label = props.label

        -- , onPress = props.onPress
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
        { buttonState : Animator.Timeline State -- (Dict Id State)

        -- , id : String
        , onPress : Maybe msg
        }


init :
    { onPress : Maybe msg

    -- , id : String
    }
    -> Model msg
init props =
    Model
        { buttonState =
            Animator.init Default

        -- Dict.fromList
        -- [ ( props.id, Default )
        -- ]
        -- , id = props.id
        , onPress = props.onPress
        }



--- Animator ---


animator : Animator.Animator (Model msg)
animator =
    Animator.animator
        |> Animator.watching
            (\(Model model) -> model.buttonState)
            (\newButtonState (Model model) ->
                Model { model | buttonState = newButtonState }
            )



-- (\buttonState ->
--     List.any ((==) Pressed) <| Dict.values buttonStates
-- )
--- Update ---
-- type alias Id =
--     String


type Msg
    = AnimationTick Time.Posix
      -- | ButtonPressed Id Swipe.Event
      -- | ButtonReleased Id Swipe.Event
    | ButtonPressed Swipe.Event
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

        maybeAlways value =
            Maybe.map (\_ -> value)

        -- setButtonState id newState =
        --     Dict.update id (maybeAlways newState) <|
        --         Animator.current model.buttonState
    in
    toParentModel <|
        case props.msg of
            AnimationTick newTime ->
                ( Animator.update newTime animator props.model
                , Effect.none
                )

            -- ButtonPressed id _ ->
            ButtonPressed _ ->
                ( Model
                    { model
                        | buttonState =
                            -- Animator.go Animator.veryQuickly (setButtonState id Pressed) model.buttonState
                            Animator.go Animator.veryQuickly Pressed model.buttonState
                    }
                , Effect.none
                )

            -- ButtonReleased id _ ->
            ButtonReleased _ ->
                ( Model
                    { model
                        | buttonState =
                            -- Animator.go Animator.slowly (setButtonState id Default) model.buttonState
                            Animator.go Animator.slowly Default model.buttonState
                    }
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

        -- buttonState id =
        --     Animator.current model.buttonState
        --         |> Dict.get id
        --         |> Maybe.withDefault Default
        -- bgColor id =
        --     (\buttonStates ->
        --         if (Dict.get id buttonStates |> Maybe.withDefault Default) == Pressed then
        --             CS.interactActiveLighterColor colorScheme |> toRgb |> Color.fromRgba
        --         else
        --             CS.interactActiveColor colorScheme |> toRgb |> Color.fromRgba
        --     )
        --         |> Animator.color model.buttonState
        --         |> Color.toRgba
        --         |> fromRgb
        bgColor =
            (\buttonState ->
                if buttonState == Pressed then
                    CS.interactActiveLighterColor colorScheme |> toRgb |> Color.fromRgba

                else
                    CS.interactActiveColor colorScheme |> toRgb |> Color.fromRgba
            )
                |> Animator.color model.buttonState
                |> Color.toRgba
                |> fromRgb

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
        button
            (commonAttributes
                ++ [ BG.color <| bgColor --model.id

                   --    , Events.onMouseDown <| settings.toMsg <| ButtonPressed model.id
                   --    , Events.onMouseUp <| settings.toMsg <| ButtonReleased model.id
                   --    , htmlAttribute <| Swipe.onStart <| settings.toMsg <| ButtonPressed model.id
                   --    , htmlAttribute <| Swipe.onStart (\event -> settings.toMsg <| ButtonPressed model.id event)
                   --    , htmlAttribute <| Swipe.onEnd (\event -> settings.toMsg <| ButtonReleased model.id event)
                   , htmlAttribute <| Swipe.onStart (\event -> settings.toMsg <| ButtonPressed event)
                   , htmlAttribute <| Swipe.onEnd (\event -> settings.toMsg <| ButtonReleased event)

                   --TODO: Border color...
                   ]
             -- ++ (if settings.isLightColored then
             --         CS.interactActiveLighter colorScheme
             --     else
             --         CS.interactActive colorScheme
             --    )
            )
            { onPress =
                if settings.isDisabled then
                    Nothing

                else
                    model.onPress
            , label = settings.label
            }
