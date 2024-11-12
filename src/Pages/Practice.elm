module Pages.Practice exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Components.IntCrementer as IntCrementer
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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
    | OnToggleWakelockHint


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

        OnToggleWakelockHint ->
            ( model
            , Effect.toggleWakelockHint
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
                            row [] <| Texts.cycles shared.appLanguage n [ Font.bold ]
                    , onCrement = CycleCountChanged
                    , model = model.cycleCrementer
                    }
                    |> IntCrementer.withMin 1
                    |> IntCrementer.withMax 9
                    |> IntCrementer.view shared.colorScheme (Session.remainingCycles shared.session)
                , let
                    estimate =
                        model.time
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
                  in
                  paragraph [] <|
                    Texts.estimatedEnd shared.appLanguage <|
                        el [ Font.bold ] <|
                            Texts.viewTime shared.appLanguage [ Font.size 30 ] shared.zone estimate
                ]
            , el
                [ width fill
                , inFront <|
                    {- Can't use "below" because elm-ui paints these in front of all other content
                       so that it stays visible during page fading. Instead we use inFront and moveDown...
                    -}
                    case shared.iOSVersion of
                        Nothing ->
                            none

                        _ ->
                            el [ moveDown 80 ] <|
                                Input.button []
                                    { onPress = Just OnToggleWakelockHint
                                    , label =
                                        column
                                            [ spacing 10
                                            , paddingEach { left = 30, right = 0, top = 0, bottom = 0 }
                                            , Font.size 12
                                            , centerX
                                            , Font.alignLeft
                                            , transparent <| not shared.showWakelockNote
                                            ]
                                        <|
                                            Texts.wakeLockNote shared.appLanguage
                                    }
                ]
                (Button.new
                    { onPress = OnStartButton
                    , label = text <| Texts.startSession shared.appLanguage
                    , model = model.startButton
                    }
                    |> Button.view shared.colorScheme
                )
            ]
    }


viewWarnings : Shared.Model -> Layouts.BaseLayout.Overlay Msg
viewWarnings shared =
    Layouts.BaseLayout.InfoWindow
        { header = Texts.warnings shared.appLanguage
        , onClose = OnToggleWarnings
        , info = column [ spacing 15 ] <| Texts.practiceWarnings shared.appLanguage
        }
