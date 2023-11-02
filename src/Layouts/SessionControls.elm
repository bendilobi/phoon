module Layouts.SessionControls exposing (Model, Msg, Props, layout)

import Browser.Navigation
import Components.StatelessAnimatedButton as Button
import Date
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Layout exposing (Layout)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Swipe as Swipe
import Route exposing (Route)
import Shared
import Task
import View exposing (View)


type alias Props =
    { showCurrentCycle : Maybe Int

    --TODO: Die gesamten Session Controls (Buttons oben und unten) in der Page
    --      definieren und hier übergeben -> route.path-basierte Verzweigungen
    --      fallen hier weg. Das Layout vielleicht umbenennen zu "SessionLayout"
    --      oder so...
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update props shared route
        , view = view props shared route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { gesture : Swipe.Gesture
    , controlsShown : Bool
    , confirmDialogShown : Bool
    , debounceBlock : Bool
    , cancelButton : Button.Model
    , discardButton : Button.Model
    , confirmButton : Button.Model
    , addCycleButton : Button.Model
    , saveButton : Button.Model
    , reloadButton : Button.Model
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
      , confirmDialogShown = False
      , debounceBlock = False
      , cancelButton = Button.init
      , discardButton = Button.init
      , confirmButton = Button.init
      , addCycleButton = Button.init
      , saveButton = Button.init
      , reloadButton = Button.init
      }
    , Effect.batch
        [ Effect.setWakeLock
        , Effect.sendCmd <| Task.perform AdjustToday Date.today
        ]
    )



-- UPDATE


type Msg
    = Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | OnCancelButton Button.Model
    | OnDiscardButton Button.Model
    | OnConfirmButton Button.Model
    | OnAddCycleButton Button.Model
    | OnSaveButton Button.Model
    | ReleaseDebounceBlock
    | AdjustToday Date.Date
    | OnReloadButton Button.Model
      -- To simulate gestures via buttons for debugging in desktop browser:
    | MouseNavTap
    | MouseNavSwipe


update : Props -> Shared.Model -> Route () -> Msg -> Model -> ( Model, Effect Msg )
update props shared route msg model =
    case msg of
        Swipe touch ->
            ( { model | gesture = Swipe.record touch model.gesture }
            , Effect.none
            )

        SwipeEnd touch ->
            let
                gesture =
                    Swipe.record touch model.gesture

                multitouchRegistered =
                    not model.controlsShown
                        && not model.debounceBlock
                        && (Swipe.maxFingers gesture == 2)

                singleTapRegistered =
                    not model.controlsShown
                        && Swipe.isTap gesture
                        && (Swipe.maxFingers gesture == 1)

                swipeSize =
                    shared.deviceInfo.window.width * 0.75
            in
            ( { model
                | gesture = Swipe.blanco
                , controlsShown = Swipe.isRightSwipe swipeSize gesture
                , debounceBlock = model.debounceBlock || multitouchRegistered
                , confirmDialogShown = False
              }
              -- TODO: Herausfinden, ob ich doch irgendwie ein sauberes "Mehr als 1 Finger beteiligt" hinkriege...
              --       Was aktuell zu passieren scheint: Beim Lupfen eines Fingers wird ein End Event
              --       ausgelöst. Wenn sich dann die zwei liegengebliebenen Finger kurz bewegen, gibts
              --       ein zweites End Event...
            , if multitouchRegistered then
                Effect.batch <|
                    (Effect.sendCmd <| Delay.after 1500 ReleaseDebounceBlock)
                        :: multitouchEffects shared route

              else if singleTapRegistered && route.path == Session.phasePath Session.Start then
                Effect.playSound Session.StartSound

              else
                Effect.none
            )

        ReleaseDebounceBlock ->
            ( { model | debounceBlock = False }, Effect.none )

        AdjustToday today ->
            ( model
            , Effect.adjustToday today
            )

        OnCancelButton newState ->
            ( { model | cancelButton = newState }
            , if newState /= Button.Released then
                Effect.none

              else if
                route.path
                    == Session.phasePath Session.Start
                    && not (shared.previousPath == Session.phasePath Session.End)
              then
                Effect.navigate shared.previousPath

              else if route.path == Session.phasePath Session.End then
                Effect.sessionEnded Session.Cancelled

              else
                Effect.batch
                    [ if route.path == Session.phasePath Session.Retention then
                        --- The user cancelled the retention, so we reset the counter in case
                        --- he retries the round by adding another round:
                        Effect.resultsUpdated <| SessionResults.resetCurrentRetention shared.results

                      else
                        Effect.none
                    , Effect.cancelSession shared.session
                    ]
            )

        OnDiscardButton newState ->
            ( { model
                | discardButton = newState
                , confirmDialogShown = newState == Button.Released
              }
            , Effect.none
            )

        OnConfirmButton newState ->
            ( { model
                | confirmButton = newState
              }
            , if newState == Button.Released then
                Effect.sessionEnded Session.Cancelled

              else
                Effect.none
            )

        OnAddCycleButton newState ->
            let
                newSession =
                    Session.withCycles 1 shared.session
            in
            ( { model
                | controlsShown = newState /= Button.Released
                , addCycleButton = newState
              }
            , if newState == Button.Released then
                Effect.batch
                    [ Effect.sessionUpdated newSession
                    , Effect.navigate <| Session.currentPath newSession
                    ]

              else
                Effect.none
            )

        OnSaveButton newState ->
            ( { model | saveButton = newState }
            , if newState == Button.Released then
                Effect.sessionEnded Session.Finished

              else
                Effect.none
            )

        MouseNavSwipe ->
            ( { model | controlsShown = True }, Effect.none )

        MouseNavTap ->
            ( { model | controlsShown = False }
            , if not model.controlsShown then
                Effect.batch <| multitouchEffects shared route

              else
                Effect.none
            )

        OnReloadButton newState ->
            --TODO: Will ich diese Funktionalität behalten? Wenn ja:
            --      Beim Reload sicherstellen, dass die vom Nutzer gewählte
            --      Rundenzahl berücksichtigt wird (-> ins localStorage...)
            ( { model | reloadButton = newState }
            , if newState == Button.Released then
                Effect.sendCmd Browser.Navigation.reload

              else
                Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


multitouchEffects : Shared.Model -> Route () -> List (Effect msg)
multitouchEffects shared route =
    [ if route.path == Session.phasePath Session.Retention then
        --- The user left the retention by multitouch, so we add the data
        Effect.resultsUpdated <| SessionResults.addRetention shared.results

      else
        Effect.none
    , Effect.navigateNext shared.session
    ]



-- VIEW


view : Props -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff Session"

    --TODO: die content.attributes hierhin?
    , attributes = []
    , element =
        el
            (content.attributes
                ++ [ width fill
                   , height fill
                   , inFront <|
                        column
                            [ width fill
                            , height fill
                            ]
                            [ if model.controlsShown && route.path == Session.phasePath Session.End then
                                viewAddCycleControls shared.colorScheme model
                                    |> map toContentMsg

                              else if
                                model.controlsShown
                                    && route.path
                                    == Session.phasePath Session.Start
                                    && SessionResults.finishedCycles shared.results
                                    == 0
                              then
                                viewReloadButton shared.colorScheme model.reloadButton
                                    |> map toContentMsg

                              else
                                none
                            , viewTouchOverlay Shared.showDebugButtons
                                |> map toContentMsg
                            , if model.controlsShown then
                                viewSessionControls shared model route
                                    |> map toContentMsg

                              else
                                none
                            ]
                   ]
            )
        <|
            column
                [ width fill
                , height fill
                ]
                [ case props.showCurrentCycle of
                    Nothing ->
                        none

                    Just cycle ->
                        el
                            ([ width fill
                             , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                             ]
                                ++ CS.primary
                            )
                        <|
                            el
                                [ centerX
                                , padding 10
                                , Font.size 30
                                ]
                            <|
                                text <|
                                    "Runde "
                                        ++ String.fromInt cycle
                , content.element
                ]
    }


viewTouchOverlay : Bool -> Element Msg
viewTouchOverlay debug =
    el
        [ width fill
        , height fill
        , htmlAttribute <| Swipe.onStart Swipe
        , htmlAttribute <| Swipe.onMove Swipe
        , htmlAttribute <| Swipe.onEnd SwipeEnd
        ]
    <|
        if debug then
            column
                [ spacing 10 ]
                [ viewDebugButton MouseNavSwipe "Swipe"
                , viewDebugButton MouseNavTap "Tap"
                ]

        else
            none


viewDebugButton : Msg -> String -> Element Msg
viewDebugButton msg label =
    button
        [ BG.color <| rgb255 33 33 33
        , Font.color <| rgb 1 1 1
        , padding 10
        , width fill
        , Font.center
        , Font.size 10
        ]
        { onPress = Just msg
        , label = text label
        }


viewAddCycleControls : ColorScheme -> Model -> Element Msg
viewAddCycleControls colorScheme model =
    el
        [ width fill
        , padding 50
        , behindContent <| el ([ alpha 0.5, width fill, height fill ] ++ CS.primary) none
        ]
        (Button.new
            { model = model.addCycleButton
            , label = text "Noch 'ne Runde"
            , onPress = OnAddCycleButton
            }
            |> Button.withLightColor
            |> Button.view colorScheme
        )


viewReloadButton : ColorScheme -> Button.Model -> Element Msg
viewReloadButton colorScheme reloadButton =
    el
        [ padding 50
        , centerX
        , width fill
        , behindContent <| el ([ alpha 0.5, width fill, height fill ] ++ CS.primary) none
        ]
    <|
        el
            [ centerX ]
        <|
            (Button.new
                { onPress = OnReloadButton
                , label = text "Reload (Sound fix)"
                , model = reloadButton
                }
                |> Button.withInline
                |> Button.view colorScheme
            )


viewSessionControls : Shared.Model -> Model -> Route () -> Element Msg
viewSessionControls shared model route =
    column
        [ width fill
        , paddingEach { bottom = 100, top = 50, left = 50, right = 50 }
        , behindContent <| el ([ alpha 0.5, width fill, height fill ] ++ CS.primary) none
        , spacing 20
        ]
    <|
        if
            route.path
                == Session.phasePath Session.End
                && SessionResults.finishedCycles shared.results
                > 0
        then
            if model.confirmDialogShown then
                [ paragraph [ Font.center ] [ text "Retentionsdaten dieser Sitzung wirklich verwerfen?" ]
                , Button.new
                    { model = model.confirmButton
                    , label = text "Ja"
                    , onPress = OnConfirmButton
                    }
                    |> Button.withLightColor
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
                , el [ height <| px 50 ] none
                , el [ centerX ] <|
                    (Button.new
                        { model = model.discardButton
                        , label = text "Sitzung verwerfen"
                        , onPress = OnDiscardButton
                        }
                        |> Button.withInline
                        |> Button.withLightColor
                        |> Button.view shared.colorScheme
                    )
                ]

        else
            [ Button.new
                { model = model.cancelButton
                , label =
                    if
                        route.path
                            == Session.phasePath Session.End
                    then
                        text "Beenden"

                    else
                        text "Sitzung abbrechen"
                , onPress = OnCancelButton
                }
                |> Button.view shared.colorScheme
            ]
