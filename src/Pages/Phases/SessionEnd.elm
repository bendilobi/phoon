module Pages.Phases.SessionEnd exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Components.Dialog as Dialog
import Components.RetentionChart as RetentionChart
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Layouts.BaseLayout
import Layouts.BaseLayout.SessionControls as SessionControls
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading as Fading exposing (Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Session as Session
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Page exposing (Page)
import Route exposing (Route)
import Shared
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
        { currentCycle = SessionResults.finishedCycles shared.results
        , controlsTop = [ viewAddCycleButton shared model ]
        , controlsBottom = viewControlsBottom shared model
        , fadeOut = model.fadeOut
        , overlay =
            if model.confirmDialogShown then
                Dialog.new
                    { header = "Retentionsdaten verwerfen?"
                    , screenWidth = shared.deviceInfo.window.width
                    , message = paragraph [] [ text "Retentionsdaten aus dieser Sitzung wirklich verwerfen?" ]
                    , choices =
                        [ Dialog.choice
                            { label = "Verwerfen"
                            , onChoose = OnConfirmButton
                            }
                        , Dialog.choice
                            { label = "Behalten"
                            , onChoose = OnDialogCancel
                            }
                        ]
                    }
                    |> Dialog.view shared.colorScheme
                    |> Layouts.BaseLayout.ModalDialog

            else
                Layouts.BaseLayout.NoOverlay
        , multitouchEffects =
            [ Effect.sendCmd <| Delay.after Fading.duration SessionControls.SessionFadedOut
            , Effect.navigateNext shared.session
            ]
        , singleTapEffects = []
        }



-- INIT


type alias Model =
    { addCycleButton : Button.Model
    , confirmButton : Button.Model
    , discardButton : Button.Model
    , confirmDialogShown : Bool
    , cancelButton : Button.Model
    , fadeOut : Fading.Trigger
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { addCycleButton = Button.init
      , confirmButton = Button.init
      , discardButton = Button.init
      , confirmDialogShown = False
      , cancelButton = Button.init
      , fadeOut = NoFade
      }
    , Effect.batch
        [ Effect.playSound Session.EndSound
        , Effect.setMotivationData <|
            MotivationData.update
                shared.results
                shared.today
                shared.sessionSettings.practiceFrequencyTarget
                shared.motivationData
        ]
    )



-- UPDATE


type Msg
    = OnAddCycleButton Button.Model
    | OnDiscardButton Button.Model
    | OnDialogCancel
    | OnConfirmButton
    | OnCancelButton Button.Model
    | FadeOutFinished Session.EndType


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OnAddCycleButton newState ->
            let
                newSession =
                    Session.withCycles 1 shared.session
            in
            ( { model | addCycleButton = newState }
            , if newState == Button.Triggered then
                Effect.batch
                    [ Effect.sessionUpdated newSession
                    , Effect.setMotivationData shared.previousMotivationData
                    , Effect.navigate NoFade <| Session.currentPath newSession
                    ]

              else
                Effect.none
            )

        OnConfirmButton ->
            ( { model
                | fadeOut = FadeWith Fading.sessionFadingColor
                , confirmDialogShown = False
              }
            , Effect.sendCmd <| Delay.after Fading.duration <| FadeOutFinished Session.Discarded
            )

        OnDiscardButton newState ->
            ( { model
                | discardButton = newState
                , confirmDialogShown = newState == Button.Triggered
              }
            , Effect.none
            )

        OnDialogCancel ->
            ( { model | confirmDialogShown = False }
            , Effect.none
            )

        OnCancelButton newState ->
            ( { model
                | cancelButton = newState
                , fadeOut =
                    if newState == Button.Triggered then
                        FadeWith Fading.sessionFadingColor

                    else
                        NoFade
              }
            , if newState == Button.Triggered then
                Effect.sendCmd <| Delay.after Fading.duration <| FadeOutFinished Session.Discarded

              else
                Effect.none
            )

        FadeOutFinished endType ->
            ( model
            , Effect.batch
                [ Effect.playSound Session.EndSound
                , Effect.sessionEnded endType
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Session End"
    , attributes =
        CS.phaseSessionEnd shared.colorScheme
    , element =
        -- el [ height fill, width fill ] <|
        column
            [ width fill
            , centerY
            , spacing 30
            ]
        <|
            (el
                [ centerX

                -- , centerY
                , Font.bold
                , Font.size 40
                ]
             <|
                text "Sitzung beendet!"
            )
                :: (case ( SessionResults.getRetentionTimes shared.results, SessionResults.meanRetentionTime shared.results ) of
                        ( Just times, Just meanTime ) ->
                            [ viewRetentionTimes times meanTime
                            , case shared.motivationData of
                                Nothing ->
                                    none

                                Just motData ->
                                    el [ centerX, Font.size 15 ] <|
                                        (RetentionChart.new
                                            { width = (shared.deviceInfo.window.width |> round) - (SafeArea.maxX shared.safeAreaInset * 2) - 40
                                            , height = 200
                                            , meanRetentionTimes = MotivationData.meanRetentionTimes motData |> List.reverse |> List.map Millis.toSeconds
                                            , maxRetention = MotivationData.maxRetention motData |> Millis.toSeconds
                                            , meanRetentionColor = CS.phaseRelaxRetentionColor shared.colorScheme
                                            , maxRetentionColor = CS.phaseRelaxRetentionColor shared.colorScheme
                                            , copyColor = CS.phaseRelaxRetentionColor shared.colorScheme
                                            }
                                            |> RetentionChart.view
                                        )
                            , paragraph
                                [ Font.center
                                , paddingXY 30 0
                                , Font.size 15
                                ]
                                [ text "Mittlere Retentionsdauern der letzten Übungen (max 30)" ]
                            ]

                        ( _, _ ) ->
                            []
                   )
    }


viewRetentionTimes : List Int -> Int -> Element msg
viewRetentionTimes times meanTime =
    -- case SessionResults.getRetentionTimes results of
    --     Nothing ->
    --         none
    --     Just times ->
    column
        [ spacing 10
        , centerX

        -- , centerY
        , Font.alignRight
        ]
    <|
        List.map2
            (\i t ->
                row [ width fill ]
                    [ el [ width fill ] <| text <| "Runde " ++ String.fromInt i ++ ": "
                    , el [ Font.bold ] <| text <| formatRetentionTime t
                    ]
            )
            (List.range 1 (List.length times))
            times
            ++ [ row
                    [ width fill
                    , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                    , paddingXY 0 7
                    ]
                    [ el [ width fill ] <| text "Durchschnitt: "
                    , el
                        [ Font.bold
                        ]
                      <|
                        text <|
                            (-- SessionResults.meanRetentionTime results
                             -- |> Maybe.withDefault 0
                             meanTime
                                |> formatRetentionTime
                            )
                    ]
               ]


formatRetentionTime : Int -> String
formatRetentionTime seconds =
    String.join ":"
        [ String.padLeft 1 '0' <| String.fromInt <| remainderBy 60 (seconds // 60)
        , String.padLeft 2 '0' <| String.fromInt <| remainderBy 60 seconds
        ]


viewAddCycleButton : Shared.Model -> Model -> Element Msg
viewAddCycleButton shared model =
    Button.new
        { model = model.addCycleButton
        , label = text "Noch 'ne Runde"
        , onPress = OnAddCycleButton
        }
        |> Button.withLightColor
        |> Button.view shared.colorScheme


viewControlsBottom : Shared.Model -> Model -> List (Element Msg)
viewControlsBottom shared model =
    if SessionResults.finishedCycles shared.results > 0 then
        [ Button.new
            { model = model.discardButton
            , label = text "Sitzung verwerfen"
            , onPress = OnDiscardButton
            }
            |> Button.withTransparent
            |> Button.withLightColor
            |> Button.view shared.colorScheme
        ]

    else
        [ Button.new
            { model = model.cancelButton
            , label = text "Beenden"
            , onPress = OnCancelButton
            }
            |> Button.withLightColor
            |> Button.view shared.colorScheme
        ]
