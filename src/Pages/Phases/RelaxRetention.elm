module Pages.Phases.RelaxRetention exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
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
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.BaseLayout_SessionControls
        { currentCycle = SessionResults.finishedCycles shared.results
        , controlsTop = []
        , controlsBottom = [ viewCancelButton shared model ]
        , fadeOut = NoFade
        , overlay = Layouts.BaseLayout.NoOverlay
        , multitouchEffects = [ Effect.navigateNext shared.session ]
        , singleTapEffects = []
        }



-- INIT


{-| Elm Time bundles timers with the same time setting together, so the
Tick timespan continues
from the previous page (Retention phase). Because of that,
the timer here wouldn't start with a full second but with what
remains from the Tick.every of the previous page.
See <https://github.com/elm/time/issues/25>

We solve it here by having a Starting step, sending the first
Tick in the init function and starting the subscription to
Time.every only if the model is Counting.

-}
type Timer
    = Starting
    | Counting Int


type alias Model =
    { timer : Timer
    , cancelButton : Button.Model
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { timer = Starting
      , cancelButton = Button.init
      }
    , Effect.batch
        [ Effect.playSound Session.RelaxRetentionSound
        , Effect.sendMsg <| Tick <| Time.millisToPosix 0
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | OnCancelButton Button.Model


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            let
                seconds =
                    case model.timer of
                        Starting ->
                            Session.relaxRetDuration shared.session
                                |> Millis.toSeconds

                        Counting sec ->
                            sec - 1
            in
            ( { model | timer = Counting seconds }
            , if seconds == 0 then
                Effect.navigateNext shared.session

              else
                Effect.none
            )

        OnCancelButton newState ->
            ( { model | cancelButton = newState }
            , if newState == Button.Triggered then
                Effect.cancelSession shared.session

              else
                Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timer of
        Starting ->
            Sub.none

        Counting _ ->
            Time.every 1000 Tick



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Relax Retention Phase"
    , attributes =
        CS.phaseRelaxRetention shared.colorScheme
    , element =
        el [ width fill, height fill ] <|
            el
                [ Font.size 120
                , Font.bold

                -- , centerX
                , Font.center
                , width fill
                , inFront <|
                    column
                        [ spacing 20

                        -- , paddingEach { left = 70, right = 70, top = 200, bottom = 0 }
                        , paddingXY 30 200
                        , Font.size 15
                        , transparent True
                        ]
                        [ bullet <| text "Halte die Luft an bis der Timer abgelaufen ist und die Glocke klingt"
                        ]
                , centerY
                ]
            <|
                text <|
                    case model.timer of
                        Starting ->
                            ""

                        Counting sec ->
                            String.fromInt sec
    }


viewCancelButton : Shared.Model -> Model -> Element Msg
viewCancelButton shared model =
    Button.new
        { model = model.cancelButton
        , label = text "Sitzung abbrechen"
        , onPress = OnCancelButton
        }
        |> Button.withLightColor
        |> Button.view shared.colorScheme
