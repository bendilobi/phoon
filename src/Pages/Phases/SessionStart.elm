module Pages.Phases.SessionStart exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Layouts
import Layouts.BaseLayout
import Layouts.BaseLayout.SessionControls as SessionControls
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.PageFading as Fading exposing (Trigger(..))
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Texts as Texts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Simple.Transition as Transition
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.BaseLayout_SessionControls
        { currentCycle = SessionResults.finishedCycles shared.results
        , controlsTop = [ viewPlayBreathingSoundsButton shared model ]
        , controlsBottom = [ viewCancelButton shared model ]
        , fadeOut = model.fadeOut
        , overlay = Layouts.BaseLayout.NoOverlay
        , goNextEffects = [ Effect.navigateNext shared.session ]
        , pageActionEffects = [ Effect.playSound Session.StartSound ]
        , sessionHints = viewSessionHints shared
        , nudgeSessionHints = shared.previousPath == Route.Path.Practice
        }



-- INIT


type alias Model =
    { bubble : Bubble.Model Msg
    , ticks : Int
    , cancelButton : Button.Model
    , fadeOut : Fading.Trigger
    , fadeInFinished : Bool
    , breathingSoundsToggleButton : Button.Model
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { ticks = 0
      , bubble =
            Bubble.init
                { bubbleType = Bubble.Static
                , onFinished = Nothing
                , breathingSpeed = Session.speedMillis shared.session
                , startWithInhale = True
                }
      , cancelButton = Button.init
      , fadeOut = NoFade
      , fadeInFinished = False
      , breathingSoundsToggleButton = Button.init
      }
    , Effect.batch
        [ Effect.sendCmd <| Delay.after (Fading.duration + 500) FadeInFinished
        , Effect.getSessionHintsHeight
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | OnCancelButton Button.Model
    | FadeOutFinished
    | FadeInFinished
    | OnToggleBreathingSoundsButton Button.Model


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            ( { model
                | ticks = model.ticks + 1
              }
            , Effect.none
            )

        OnCancelButton newState ->
            ( { model
                | cancelButton = newState
                , fadeOut =
                    if shared.previousPath == Session.phasePath Session.End then
                        NoFade

                    else if newState == Button.Triggered then
                        FadeWith Fading.sessionFadingColor

                    else
                        NoFade
              }
            , if newState == Button.Triggered then
                if shared.previousPath == Session.phasePath Session.End then
                    Effect.cancelSession shared.session

                else
                    Effect.sendCmd <| Delay.after Fading.duration FadeOutFinished

              else
                Effect.none
            )

        OnToggleBreathingSoundsButton newState ->
            let
                settings =
                    shared.sessionSettings

                soundsEnabled =
                    not settings.playBreathingSounds
            in
            ( { model
                | breathingSoundsToggleButton = newState

                --TODO: Implement playing sounds also if bubble doesn't count breaths
                -- , bubble =
                --     if newState == Button.Triggered then
                --         model.bubble |> Bubble.withBreathingSounds soundsEnabled
                --     else
                --         model.bubble
              }
            , if newState == Button.Triggered then
                Effect.batch
                    [ Effect.updateSessionSettings { settings | playBreathingSounds = soundsEnabled }
                    , Effect.sessionUpdated <| Session.withPlayBreathingSounds soundsEnabled shared.session
                    ]

              else
                Effect.none
            )

        FadeInFinished ->
            ( { model | fadeInFinished = True }, Effect.none )

        FadeOutFinished ->
            ( model
            , Effect.navigate (FadeWith Fading.sessionFadingColor) shared.previousPath
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    if model.fadeInFinished then
        {- We use the Ticks to synchronize the appareance of icons to the Bubble.
           The bubble itself animates without external input...
        -}
        --- Reducing the tickSpeed by 1 millisecond is a fix for a bug in Elm:
        --- https://github.com/elm/time/issues/25
        Time.every (Bubble.tickSpeed model.bubble - 1) Tick

    else
        Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Texts.practiceSetup shared.appLanguage
    , attributes =
        CS.phaseSessionStart shared.colorScheme
    , element =
        let
            container =
                if shared.deviceInfo.orientation == Portrait then
                    column

                else
                    row

            bubbleSize =
                min shared.deviceInfo.window.width
                    shared.deviceInfo.window.height
                    * 0.5
                    |> round
        in
        container
            [ width fill
            , height fill
            , spacing 100
            ]
            [ el [ height fill, width fill ] none
            , el
                [ centerX
                , centerY
                ]
              <|
                viewReminder shared model 2 FeatherIcons.volume2
            , el [ centerX, centerY, width <| px bubbleSize, height <| px bubbleSize ] <|
                if model.fadeInFinished then
                    Bubble.new
                        { model = model.bubble
                        , size = bubbleSize
                        , bubbleColor = CS.phaseSessionStartCopyColor shared.colorScheme
                        , bgColor = CS.phaseSessionStartColor shared.colorScheme
                        }
                        |> Bubble.withLabel "Start"
                        |> Bubble.withFontSize 50
                        |> Bubble.view

                else
                    none
            , el
                [ centerX
                , centerY
                ]
              <|
                viewReminder shared model 3 FeatherIcons.bellOff
            , el
                [ width fill
                , height fill
                ]
                none
            ]
    }


viewReminder : Shared.Model -> Model -> Int -> FeatherIcons.Icon -> Element msg
viewReminder shared model ticks icon =
    if shared.previousPath == Route.Path.Practice then
        row
            [ spacing 10
            , Font.size 30
            , alpha <|
                if model.ticks < ticks then
                    0

                else
                    1
            , htmlAttribute <| Transition.properties [ Transition.opacity 700 [ Transition.easeIn ] ]
            ]
            [ el [] <|
                html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 30 icon
            , text "?"
            ]

    else
        none


viewPlayBreathingSoundsButton : Shared.Model -> Model -> Element Msg
viewPlayBreathingSoundsButton shared model =
    Button.new
        { model = model.breathingSoundsToggleButton
        , label =
            text <|
                if Session.playBreathingSounds shared.session then
                    Texts.muteBreathingSounds shared.appLanguage

                else
                    Texts.playBreathingSounds shared.appLanguage
        , onPress = OnToggleBreathingSoundsButton
        }
        |> Button.view shared.colorScheme


viewCancelButton : Shared.Model -> Model -> Element Msg
viewCancelButton shared model =
    Button.new
        { model = model.cancelButton
        , label = text <| Texts.cancelSession shared.appLanguage
        , onPress = OnCancelButton
        }
        |> Button.view shared.colorScheme


viewSessionHints : Shared.Model -> SessionControls.SessionHints msg
viewSessionHints shared =
    { heading = Texts.practiceSetup shared.appLanguage
    , content =
        column
            [ spacing 20 ]
        <|
            Texts.sessionStartHintsIntro shared.appLanguage
                ++ (if shared.pointerIsMouse == Just True then
                        Texts.sessionStartHintsKey shared.appLanguage

                    else
                        []
                   )
                ++ (Texts.sessionStartHintsBullets shared.appLanguage <| Texts.keyWrapper shared.pointerIsMouse)
    }
