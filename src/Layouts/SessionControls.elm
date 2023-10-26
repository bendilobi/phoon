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
    { showSessionProgress : Bool }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update shared route
        , view = view props shared route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { gesture : Swipe.Gesture
    , controlsShown : Bool
    , debugButtonsShown : Bool
    , debounceBlock : Bool
    , cancelButton : Button.Model
    , addCycleButton : Button.Model
    , saveButton : Button.Model
    , reloadButton : Button.Model
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
      , debugButtonsShown = False
      , debounceBlock = False
      , cancelButton = Button.init
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
    | OnAddCycleButton Button.Model
    | OnSaveButton Button.Model
    | ReleaseDebounceBlock
    | AdjustToday Date.Date
    | OnReloadButton Button.Model
      -- To simulate gestures via buttons for debugging in desktop browser:
    | MouseNavTap
    | MouseNavSwipe


update : Shared.Model -> Route () -> Msg -> Model -> ( Model, Effect Msg )
update shared route msg model =
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
            in
            ( { model
                | gesture = Swipe.blanco
                , controlsShown = Swipe.isRightSwipe 300 gesture
                , debounceBlock = model.debounceBlock || multitouchRegistered
              }
              -- TODO: Herausfinden, ob ich doch irgendwie ein sauberes "Mehr als 1 Finger beteiligt" hinkriege...
              --       Was aktuell zu passieren scheint: Beim Lupfen eines Fingers wird ein End Event
              --       ausgelöst. Wenn sich dann die zwei liegengebliebenen Finger kurz bewegen, gibts
              --       ein zweites End Event...
            , if multitouchRegistered then
                Effect.batch
                    [ Effect.sendCmd <| Delay.after 1500 ReleaseDebounceBlock
                    , Effect.navigateNext shared.session
                    ]

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
            let
                sessionAtEnd =
                    Session.jumpToEnd shared.session
            in
            ( { model
                | controlsShown = newState == Button.Pressed
                , cancelButton = newState
              }
            , if newState == Button.Pressed then
                Effect.none

              else if
                --TODO: Wie kann die Logik hier verständlicher und robuster gemacht werden?
                route.path
                    == Session.phasePath Session.Start
                    && not (shared.previousPath == Session.phasePath Session.End)
              then
                Effect.navigate shared.previousPath

              else if route.path == Session.phasePath Session.End then
                Effect.sessionEnded Session.Cancelled

              else
                Effect.batch
                    [ Effect.sessionUpdated sessionAtEnd
                    , Effect.navigate <| Session.currentPath sessionAtEnd
                    ]
            )

        OnAddCycleButton newState ->
            let
                newSession =
                    Session.withCycles 1 shared.session
            in
            ( { model
                | controlsShown = newState == Button.Pressed
                , addCycleButton = newState
              }
            , if newState == Button.Pressed then
                Effect.none

              else
                Effect.batch
                    [ Effect.sessionUpdated newSession
                    , Effect.navigate <| Session.currentPath newSession
                    ]
            )

        OnSaveButton newState ->
            ( { model | saveButton = newState }
            , if newState == Button.Pressed then
                Effect.none

              else
                Effect.sessionEnded Session.Finished
            )

        MouseNavSwipe ->
            ( { model | controlsShown = True }, Effect.none )

        MouseNavTap ->
            ( { model | controlsShown = False }
            , if not model.controlsShown then
                Effect.navigateNext shared.session

              else
                Effect.none
            )

        OnReloadButton newState ->
            ( { model | reloadButton = newState }
            , if newState == Button.Default then
                Effect.sendCmd Browser.Navigation.reload

              else
                Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Props -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff Session"
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

                              else if model.controlsShown && route.path == Session.phasePath Session.Start then
                                viewReloadButton shared.colorScheme model.reloadButton
                                    |> map toContentMsg

                              else
                                none
                            , viewTouchOverlay model.debugButtonsShown
                                |> map toContentMsg
                            , if model.controlsShown then
                                viewSessionControls shared.colorScheme model route
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
                [ if props.showSessionProgress then
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
                                    ++ (String.fromInt <|
                                            SessionResults.finishedCycles shared.results
                                                + 1
                                       )

                  else
                    none
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
    el [ width fill, padding 50 ]
        (Button.new
            { model = model.addCycleButton
            , label = text "Noch 'ne Runde"
            , onPress = OnAddCycleButton
            }
            |> Button.view colorScheme
        )


viewReloadButton : ColorScheme -> Button.Model -> Element Msg
viewReloadButton colorScheme reloadButton =
    el [ padding 50, centerX ]
        (Button.new
            { onPress = OnReloadButton
            , label = text "Reload (Sound fix)"
            , model = reloadButton
            }
            |> Button.withInline
            |> Button.view colorScheme
        )


viewSessionControls : ColorScheme -> Model -> Route () -> Element Msg
viewSessionControls colorScheme model route =
    column
        [ width fill
        , paddingEach { bottom = 100, top = 0, left = 50, right = 50 }
        ]
    <|
        if route.path == Session.phasePath Session.End then
            [ Button.new
                { model = model.saveButton
                , label = text "Speichern & beenden"
                , onPress = OnSaveButton
                }
                |> Button.view colorScheme
            , el [ height <| px 70 ] none
            , el [ centerX ] <|
                (Button.new
                    { model = model.cancelButton
                    , label = text "Sitzung verwerfen"
                    , onPress = OnCancelButton
                    }
                    |> Button.withInline
                    |> Button.view colorScheme
                )
            ]

        else
            [ Button.new
                { model = model.cancelButton
                , label = text "Sitzung abbrechen"
                , onPress = OnCancelButton
                }
                |> Button.view colorScheme
            ]
