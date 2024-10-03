module Pages.Phases.Retention exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Layouts.BaseLayout
import Layouts.BaseLayout.SessionControls as SessionControls
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.PageFading exposing (Trigger(..))
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Texts as Texts
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
        , subscriptions = subscriptions
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
        , multitouchEffects =
            [ Effect.resultsUpdated <| SessionResults.addRetention shared.results
            , Effect.navigateNext shared.session
            ]
        , singleTapEffects = []
        , sessionHints = viewSessionHints shared
        , nudgeSessionHints = False
        }



-- INIT


type alias Model =
    { cancelButton : Button.Model }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { cancelButton = Button.init }
    , Effect.batch
        [ Effect.playSound Session.RetentionSound
        , Effect.getSessionHintsHeight
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
            ( model, Effect.resultsUpdated <| SessionResults.incrementCurrentRetention shared.results )

        OnCancelButton newState ->
            ( { model | cancelButton = newState }
            , if newState == Button.Triggered then
                Effect.batch
                    [ Effect.resultsUpdated <| SessionResults.resetCurrentRetention shared.results
                    , Effect.cancelSession shared.session
                    ]

              else
                Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Texts.retention shared.appLanguage
    , attributes =
        CS.phaseRetention shared.colorScheme
    , element =
        el [ width fill, height fill ] <|
            el
                [ Font.size 30
                , width fill
                , Font.center
                , centerY
                ]
            <|
                text <|
                    Millis.toString False <|
                        Millis.fromSeconds <|
                            SessionResults.currentRetentionTime shared.results
    }


viewCancelButton : Shared.Model -> Model -> Element Msg
viewCancelButton shared model =
    Button.new
        { model = model.cancelButton
        , label = text <| Texts.cancelSession shared.appLanguage
        , onPress = OnCancelButton
        }
        |> Button.withLightColor
        |> Button.view shared.colorScheme


viewSessionHints : Shared.Model -> SessionControls.SessionHints msg
viewSessionHints shared =
    { heading = Texts.retention shared.appLanguage
    , content =
        column
            [ spacing 20
            ]
        <|
            Texts.retentionHints shared.appLanguage
    }
