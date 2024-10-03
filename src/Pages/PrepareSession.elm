module Pages.PrepareSession exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Components.IntCrementer as IntCrementer
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Layouts
import Layouts.BaseLayout
import Layouts.BaseLayout.MainNav
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading as Fading exposing (Trigger(..))
import Lib.Session as Session exposing (Session)
import Lib.SessionResults as SessionResults
import Lib.Texts as Texts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Task
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
    Layouts.BaseLayout_MainNav
        { header = Just <| Texts.prepareSession shared.appLanguage
        , headerIcon = Just <| Layouts.BaseLayout.MainNav.viewHeaderButton FeatherIcons.alertTriangle OnToggleWarnings
        , enableScrolling = False
        , fadeOut = model.fadeOut
        , subPage = Nothing
        , overlay =
            case shared.infoWindowState of
                Shared.Model.Closed ->
                    Layouts.BaseLayout.NoOverlay

                _ ->
                    viewWarnings shared
        }



-- INIT


type alias Model =
    { time : Time.Posix
    , startButton : Button.Model
    , cycleCrementer : IntCrementer.Model
    , fadeOut : Fading.Trigger
    , warningWasShown : Bool
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { time = Time.millisToPosix 0
      , startButton = Button.init
      , cycleCrementer = IntCrementer.init
      , fadeOut = NoFade
      , warningWasShown = False
      }
    , Effect.batch
        [ Effect.sendCmd <| Task.perform Tick Time.now
        , Effect.sessionUpdated <| Session.new shared.sessionSettings
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | OnStartButton Button.Model
    | CycleCountChanged Int IntCrementer.Model
    | ReadyToStartSession
    | OnToggleWarnings


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Effect.none
            )

        CycleCountChanged cycles newState ->
            ( { model | cycleCrementer = newState }
            , if IntCrementer.wasTriggered newState then
                shared.session
                    |> Session.withCycles cycles
                    |> Effect.sessionUpdated

              else
                Effect.none
            )

        OnStartButton newState ->
            case newState of
                Button.Triggered ->
                    if shared.motivationData == Nothing && not model.warningWasShown then
                        ( { model
                            | startButton = newState
                            , warningWasShown = True
                          }
                        , Effect.sendMsg OnToggleWarnings
                        )

                    else
                        ( { model
                            | startButton = newState
                            , fadeOut = FadeWith Fading.sessionFadingColor
                          }
                        , Effect.sendCmd <| Delay.after Fading.duration ReadyToStartSession
                        )

                _ ->
                    ( { model | startButton = newState }, Effect.none )

        ReadyToStartSession ->
            ( model
            , Effect.batch
                [ Effect.resultsUpdated SessionResults.empty
                , Effect.playSound Session.StartSound
                , Effect.navigate (FadeWith Fading.sessionFadingColor) <|
                    Session.currentPath shared.session
                ]
            )

        OnToggleWarnings ->
            ( model
            , case shared.infoWindowState of
                Shared.Model.Closed ->
                    Effect.setInfoWindowState Shared.Model.Half

                _ ->
                    Effect.setInfoWindowState Shared.Model.Closed
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    if shared.appVisible then
        Time.every 1000 Tick

    else
        Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Texts.prepareSession shared.appLanguage
    , attributes = CS.primaryPrepareSession shared.colorScheme
    , element =
        column
            [ width fill
            , padding 20
            , Font.center
            , spacing 70
            , centerY
            ]
            [ column [ centerX, spacing 30 ]
                [ IntCrementer.new
                    { label =
                        \n ->
                            row [] <| Texts.cycles shared.appLanguage n
                    , onCrement = CycleCountChanged
                    , model = model.cycleCrementer
                    }
                    |> IntCrementer.withMin 1
                    |> IntCrementer.withMax 9
                    |> IntCrementer.view shared.colorScheme (Session.remainingCycles shared.session)

                --TODO: Zeitdarstellung in Texts übernehmen (pm/am etc.)
                , paragraph [] <| Texts.estimatedEnd shared.appLanguage <| viewEstimatedTime shared model.time
                ]
            , el [ width fill ]
                (Button.new
                    { onPress = OnStartButton
                    , label = text <| Texts.startSession shared.appLanguage
                    , model = model.startButton
                    }
                    |> Button.view shared.colorScheme
                )
            ]
    }


viewEstimatedTime : Shared.Model -> Time.Posix -> Element msg
viewEstimatedTime shared time =
    let
        estimate =
            time
                |> Time.posixToMillis
                |> (+)
                    (Session.estimatedDurationMillis
                        (shared.motivationData
                            |> Maybe.map MotivationData.meanRetentionTimes
                            |> Maybe.withDefault []
                        )
                        shared.session
                        |> Millis.toInt
                    )
                |> Time.millisToPosix

        hour =
            String.fromInt <| Time.toHour shared.zone estimate

        minute =
            Time.toMinute shared.zone estimate
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    text <| hour ++ ":" ++ minute


viewWarnings : Shared.Model -> Layouts.BaseLayout.Overlay Msg
viewWarnings shared =
    Layouts.BaseLayout.InfoWindow
        { header = Texts.warnings shared.appLanguage
        , info = text <| Texts.practiceWarnings shared.appLanguage
        , onClose = OnToggleWarnings
        }
