module Pages.Phases.SessionEnd exposing (Model, Msg, page)

import Components.AnimatedButton as Button
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.PageFading as Fading
import Lib.Session as Session
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Page exposing (Page)
import Route exposing (Route)
import Shared
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
    Layouts.SessionControls
        { showCurrentCycle = Nothing
        , controlsTop = [ viewAddCycleButton shared model ]
        , controlsBottom = viewControlsBottom shared model
        , fadeOut = model.fadeOut
        }



-- INIT


type alias Model =
    { addCycleButton : Button.Model
    , confirmButton : Button.Model
    , saveButton : Button.Model
    , discardButton : Button.Model
    , confirmDialogShown : Bool
    , cancelButton : Button.Model
    , fadeOut : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { addCycleButton = Button.init
      , confirmButton = Button.init
      , saveButton = Button.init
      , discardButton = Button.init
      , confirmDialogShown = False
      , cancelButton = Button.init
      , fadeOut = False
      }
    , Effect.playSound Session.EndSound
    )



-- UPDATE


type Msg
    = OnAddCycleButton Button.Model
    | OnConfirmButton Button.Model
    | OnSaveButton Button.Model
    | OnDiscardButton Button.Model
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
                    , Effect.navigate False <| Session.currentPath newSession
                    ]

              else
                Effect.none
            )

        OnConfirmButton newState ->
            ( { model
                | confirmButton = newState
                , fadeOut = newState == Button.Triggered
              }
            , if newState == Button.Triggered then
                Effect.sendCmd <| Delay.after Fading.fadeDuration <| FadeOutFinished Session.Cancelled

              else
                Effect.none
            )

        OnSaveButton newState ->
            ( { model
                | saveButton = newState
                , fadeOut = newState == Button.Triggered
              }
            , if newState == Button.Triggered then
                Effect.sendCmd <| Delay.after Fading.fadeDuration <| FadeOutFinished Session.Finished

              else
                Effect.none
            )

        OnDiscardButton newState ->
            ( { model
                | discardButton = newState
                , confirmDialogShown =
                    if newState == Button.Triggered then
                        not model.confirmDialogShown

                    else
                        model.confirmDialogShown
              }
            , Effect.none
            )

        OnCancelButton newState ->
            ( { model
                | cancelButton = newState
                , fadeOut = newState == Button.Triggered
              }
            , if newState == Button.Triggered then
                Effect.sendCmd <| Delay.after Fading.fadeDuration <| FadeOutFinished Session.Cancelled

              else
                Effect.none
            )

        FadeOutFinished endType ->
            ( model
            , Effect.sessionEnded endType
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
        column
            [ width fill
            , height fill
            , spacing 30
            ]
            [ el
                [ centerX
                , centerY
                , Font.bold
                , Font.size 40
                ]
              <|
                text "Sitzung beendet!"
            , viewRetentionTimes shared.results
            ]
    }


viewRetentionTimes : SessionResults -> Element msg
viewRetentionTimes results =
    case SessionResults.getRetentionTimes results of
        Nothing ->
            none

        Just times ->
            column
                [ spacing 10
                , centerX
                , centerY
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
                                    (SessionResults.meanRetentionTime results
                                        |> Maybe.withDefault 0
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
        if model.confirmDialogShown then
            [ paragraph [ Font.center ] [ text "Retentionsdaten dieser Sitzung wirklich verwerfen?" ]
            , Button.new
                { model = model.confirmButton
                , label = text "Ja"
                , onPress = OnConfirmButton
                }
                |> Button.withLightColor
                |> Button.view shared.colorScheme

            -- , el [ height <| px 10 ] none
            , Button.new
                { model = model.discardButton
                , label = text "Zurück"
                , onPress = OnDiscardButton
                }
                |> Button.withLightColor
                |> Button.withTransparent
                |> Button.view shared.colorScheme
            ]

        else
            [ Button.new
                { model = model.saveButton
                , label = text "Speichern & beenden"
                , onPress = OnSaveButton
                }
                |> Button.withLightColor
                |> Button.view shared.colorScheme

            -- , el [ height <| px 10 ] none
            , Button.new
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
