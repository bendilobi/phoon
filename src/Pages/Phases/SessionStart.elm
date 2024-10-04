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
        , controlsTop = []
        , controlsBottom = [ viewCancelButton shared model ]
        , fadeOut = model.fadeOut
        , overlay = Layouts.BaseLayout.NoOverlay
        , multitouchEffects = [ Effect.navigateNext shared.session ]
        , singleTapEffects = [ Effect.playSound Session.StartSound ]
        , sessionHints = viewSessionHints shared
        , nudgeSessionHints = shared.previousPath == Route.Path.PrepareSession
        }



-- INIT


type alias Model =
    { bubble : Bubble.Model Msg
    , ticks : Int
    , cancelButton : Button.Model
    , fadeOut : Fading.Trigger
    , fadeInFinished : Bool
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { ticks = 0
      , bubble =
            Bubble.init
                { bubbleType = Bubble.Static
                , onFinished = Nothing
                , breathingSpeed = Session.speedMillis shared.session
                }
      , cancelButton = Button.init
      , fadeOut = NoFade
      , fadeInFinished = False
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


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            let
                ( bModel, _ ) =
                    Bubble.update
                        { msg = Bubble.Tick
                        , model = model.bubble
                        , toModel = \bubble -> { model | bubble = bubble }
                        }
            in
            ( { model
                | ticks = model.ticks + 1
                , bubble = bModel.bubble
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
                viewReminder shared model 1 FeatherIcons.volume2
            , el [ width fill, height fill ] <|
                if model.fadeInFinished then
                    el [ centerX, centerY ] <|
                        (Bubble.new
                            { model = model.bubble
                            , size =
                                min shared.deviceInfo.window.width
                                    shared.deviceInfo.window.height
                                    * 0.5
                                    |> round
                            , bubbleColor = CS.phaseSessionStartCopyColor shared.colorScheme
                            , bgColor = CS.phaseSessionStartColor shared.colorScheme
                            }
                            |> Bubble.withLabel "Start"
                            |> Bubble.withFontSize 50
                            |> Bubble.view
                        )

                else
                    none
            , el
                [ width fill
                ]
              <|
                el
                    [ centerX
                    , centerY
                    ]
                <|
                    viewReminder shared model 2 FeatherIcons.bellOff
            , el
                [ width fill
                , height fill
                ]
                none
            ]
    }


viewReminder : Shared.Model -> Model -> Int -> FeatherIcons.Icon -> Element msg
viewReminder shared model ticks icon =
    if shared.previousPath == Route.Path.PrepareSession then
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
            Texts.sessionStartHints shared.appLanguage
    }
