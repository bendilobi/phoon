module Pages.Phases.Breathing exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.PageFading exposing (Trigger(..))
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Utils exposing (bullet)
import Page exposing (Page)
import Route exposing (Route)
import Shared
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
        { currentCycle = SessionResults.finishedCycles shared.results + 1
        , controlsTop = []
        , controlsBottom = [ viewCancelButton shared model ]
        , fadeOut = NoFade
        , overlay = Layouts.BaseLayout.NoOverlay
        , multitouchEffects = [ Effect.navigateNext shared.session ]
        , singleTapEffects = []
        }


type alias Model =
    { bubble : Bubble.Model Msg
    , breathingFinished : Bool
    , cancelButton : Button.Model
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { bubble =
            Bubble.init
                { bubbleType = Bubble.Counting <| Session.breathCount shared.session
                , onFinished = Just BubbleFinished
                , breathingSpeed = Session.speedMillis shared.session
                }
      , breathingFinished = False
      , cancelButton = Button.init
      }
    , Effect.batch
        [ Effect.playSound Session.BreathingSound
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | BubbleFinished
    | OnCancelButton Button.Model


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            Bubble.update
                { msg = Bubble.Tick
                , model = model.bubble
                , toModel = \bubble -> { model | bubble = bubble }
                }

        BubbleFinished ->
            ( { model | breathingFinished = True }
            , Effect.playSound Session.BreathingEndSound
            )

        OnCancelButton newState ->
            ( { model | cancelButton = newState }
            , if newState == Button.Triggered then
                Effect.cancelSession shared.session

              else
                Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Time.every (Bubble.tickSpeed model.bubble) Tick



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Atem-Phase"
    , attributes =
        CS.phaseBreathing shared.colorScheme
    , element =
        el [ width fill, height fill ] <|
            if model.breathingFinished then
                el
                    [ width fill
                    , centerY
                    , Font.size 40
                    , Font.center
                    , inFront <|
                        column
                            [ spacing 20

                            -- , paddingEach { left = 70, right = 70, top = 70, bottom = 0 }
                            , paddingXY 30 70
                            , Font.size 15
                            , moveDown 40
                            , transparent True
                            ]
                            [ bullet <| text "Atme noch einmal tief ein und lass' dann den Atem los"
                            , bullet <| text "Halte die Luft an"
                            , bullet <| text "Dann tippe mit drei Fingern um die Retention zu beginnen"
                            ]
                    ]
                <|
                    text "Retention \nvorbereiten"

            else
                let
                    window =
                        shared.deviceInfo.window

                    bubbleSize =
                        min window.width window.height * 0.9 |> round
                in
                el
                    [ centerX
                    , centerY
                    , inFront <|
                        column
                            [ spacing 20

                            -- , paddingEach { left = 70, right = 70, top = 10, bottom = 0 }
                            , paddingXY 30 10
                            , Font.size 15
                            , moveDown <| toFloat bubbleSize
                            , transparent True
                            ]
                            [ bullet <| text "Atme tief ein und aus im Rhythmus der Animation bis die Glocke klingt"
                            ]
                    ]
                <|
                    (Bubble.new
                        { model = model.bubble
                        , size = bubbleSize
                        , bubbleColor = CS.phaseSessionStartColor shared.colorScheme
                        , bgColor = CS.phaseBreathingColor shared.colorScheme
                        }
                        |> Bubble.view
                    )
    }


viewCancelButton : Shared.Model -> Model -> Element Msg
viewCancelButton shared model =
    Button.new
        { model = model.cancelButton
        , label = text "Sitzung abbrechen"
        , onPress = OnCancelButton
        }
        |> Button.view shared.colorScheme
