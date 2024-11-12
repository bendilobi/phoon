module Components.AnimatedButton exposing (Model(..), init, new, view, withDisabled, withInline, withLightColor, withTransparent)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
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
        , isTransparent : Bool
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
        , isTransparent = False
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


withTransparent : Button msg -> Button msg
withTransparent (Settings settings) =
    Settings { settings | isTransparent = True }



--- Model ---


type Model
    = Pressed Bool
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
        commonAttributes =
            [ width fill
            , padding 20
            , Font.center
            , Border.rounded 15
            ]

        animationAttributes =
            case settings.model of
                Pressed _ ->
                    [ htmlAttribute <|
                        Transition.properties
                            [ Transition.backgroundColor 200 []
                            ]
                    ]

                Cancelled ->
                    []

                _ ->
                    [ htmlAttribute <|
                        Transition.properties
                            [ Transition.backgroundColor 1000 [ Transition.easeOutQuad ]
                            ]
                    ]

        {- Unfortunately, we need quite a bit of event and state handling to have buttons that work nicely with
           touch and mouse input devices: having different transitions when pressing and releasing as well as nice
           mouse-overs.
        -}
        eventAttributes =
            {- Our main events are pointerdown and pointerup... -}
            [ pointer
            , htmlAttribute <| HEvents.on "pointerdown" <| Decode.succeed <| settings.onPress <| Pressed True

            --TODO: For some reason, when we use "pointerup" here, the buttons do not work if the user presses them
            --      longer than about a second. Maybe there is some "longpress" behavior? Using "onclick" fixes this
            --      on iOS, but on Android, the issure remains...
            -- , htmlAttribute <| HEvents.on "pointerup" <| Decode.succeed <| settings.onPress Triggered
            , Events.onClick <| settings.onPress Triggered

            {- ...but on touch devices, if the user swipes from outside of the button into it and then lifts the
               finger, we get a pointerup event. So we want to provide visual feedback to the user by showing the
               button in Pressed state:
            -}
            , htmlAttribute <| HEvents.on "pointerenter" <| Decode.succeed <| settings.onPress <| Pressed False

            {- Now, if the user swiped into the button and then swipes out of it again, the button needs to go back
               to a non-pressed state, so that it can be rendered "non-pressed".
               Unfortunately again, pointerleave is triggered also when pointerup is triggered so we have to
               distinguish between a Pressed state originating from pointerdown and one from pointerenter:
            -}
            , htmlAttribute <|
                HEvents.on "pointerleave" <|
                    Decode.succeed <|
                        settings.onPress <|
                            case settings.model of
                                Pressed False ->
                                    Cancelled

                                Pressed True ->
                                    Released

                                Cancelled ->
                                    Cancelled

                                Released ->
                                    Released

                                Triggered ->
                                    -- Triggered
                                    Cancelled

            {- And finally, we want the button to go into non-pressed state and not do a Released
               animation if the interaction is cancelled, eg. if the user started scrolling after
               pressing the button:
            -}
            , htmlAttribute <| HEvents.on "pointercancel" <| Decode.succeed <| settings.onPress Cancelled

            --TODO: Prevent buttons being triggered when the user swipes out of the button's boundaries.
            --      Seems we need to do this via javascript, thus using ports:
            --      https://developer.mozilla.org/en-US/docs/Web/API/Element/pointerdown_event)
            ]
    in
    if settings.isInline then
        if settings.isDisabled then
            el [ Font.color <| CS.interactInactiveDarkerColor colorScheme ] settings.label

        else
            el
                ([ Font.color <|
                    if settings.isLightColored then
                        CS.interactActiveLighterColor colorScheme

                    else
                        CS.interactActiveColor colorScheme
                 , pointer
                 , behindContent <|
                    --- To give it a bit of padding without affecting the layout
                    el
                        ([ Border.rounded 5
                         , padding 5
                         , moveUp 5
                         , moveLeft 5
                         ]
                            ++ ((BG.color <|
                                    case settings.model of
                                        Pressed _ ->
                                            rgba255 189 201 226 1.0

                                        _ ->
                                            rgba 189 201 226 0
                                )
                                    :: animationAttributes
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
                settings.label

    else if settings.isDisabled then
        el
            (commonAttributes ++ CS.interactInactive colorScheme)
        <|
            settings.label

    else
        {- For some reason, animations are triggered wrongly unless the button is wrapped in
           two "el"... Why???
        -}
        el [ width fill ] <|
            el [ width fill ] <|
                el
                    (commonAttributes
                        ++ (case settings.model of
                                Pressed _ ->
                                    if settings.isTransparent then
                                        [ Font.color <|
                                            if settings.isLightColored then
                                                CS.interactActiveLighterColor colorScheme

                                            else
                                                CS.interactActiveColor colorScheme
                                        , BG.color <| rgba255 189 201 226 1.0
                                        ]

                                    else if settings.isLightColored then
                                        CS.interactActive colorScheme

                                    else
                                        CS.interactActiveLighter colorScheme

                                _ ->
                                    if settings.isTransparent then
                                        [ Font.color <|
                                            if settings.isLightColored then
                                                CS.interactActiveLighterColor colorScheme

                                            else
                                                CS.interactActiveColor colorScheme
                                        , BG.color <| rgba255 189 201 226 0
                                        ]

                                    else if settings.isLightColored then
                                        CS.interactActiveLighter colorScheme

                                    else
                                        CS.interactActive colorScheme
                           )
                        ++ animationAttributes
                        ++ eventAttributes
                    )
                    settings.label
