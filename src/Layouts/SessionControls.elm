module Layouts.SessionControls exposing (Model, Msg, Props, layout)

import Components.Button
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
import Lib.Utils as Utils
import Route exposing (Route)
import Shared
import Shared.Msg
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
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
      , debugButtonsShown = False
      , debounceBlock = False
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
    | Cancelled
    | AddCycle
    | EndSession
    | ReleaseDebounceBlock
    | AdjustToday Date.Date
      -- To simulate gestures via buttons for debugging in desktop browser:
      -- TODO: Herausfinden, ob der Mobile-Simulator von Chrome multitouch kann
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
                    [ Effect.sendCmd <| Delay.after 1000 ReleaseDebounceBlock
                    , Effect.navigateNext shared.session
                    ]

              else if singleTapRegistered && route.path == Session.phasePath Session.Start then
                Effect.playSound Utils.SessionStart

              else
                Effect.none
            )

        ReleaseDebounceBlock ->
            ( { model | debounceBlock = False }, Effect.none )

        AdjustToday today ->
            ( model
            , Effect.adjustToday today
            )

        Cancelled ->
            let
                sessionAtEnd =
                    Session.jumpToEnd shared.session
            in
            ( { model | controlsShown = False }
            , if
                route.path
                    == Session.phasePath Session.Start
                    && not (shared.previousPath == Session.phasePath Session.End)
              then
                Effect.navigate shared.previousPath

              else if route.path == Session.phasePath Session.End then
                Effect.sessionEnded True

              else
                Effect.batch
                    [ Effect.sessionUpdated sessionAtEnd
                    , Effect.navigate <| Session.currentPath sessionAtEnd
                    ]
            )

        AddCycle ->
            let
                newSession =
                    Session.withCycles 1 shared.session
            in
            ( { model | controlsShown = False }
            , Effect.batch
                [ Effect.sessionUpdated newSession
                , Effect.navigate <| Session.currentPath newSession
                ]
            )

        EndSession ->
            ( model
            , Effect.sessionEnded False
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
                            [ viewTouchOverlay model.debugButtonsShown
                                |> map toContentMsg
                            , if model.controlsShown then
                                viewSessionControls shared.colorScheme route
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


viewSessionControls : ColorScheme -> Route () -> Element Msg
viewSessionControls colorScheme route =
    column
        [ width fill
        , paddingEach { bottom = 100, top = 0, left = 50, right = 50 }
        ]
    <|
        if route.path == Session.phasePath Session.End then
            [ Components.Button.new
                { onPress = Just AddCycle
                , label = text "Noch 'ne Runde"
                }
                |> Components.Button.view colorScheme
            , el [ height <| px 70 ] none
            , Components.Button.new
                { onPress = Just EndSession
                , label = text "Speichern & beenden"
                }
                |> Components.Button.view colorScheme
            , el [ height <| px 70 ] none
            , el [ centerX ] <|
                (Components.Button.new
                    { onPress = Just Cancelled
                    , label = text "Sitzung verwerfen"
                    }
                    |> Components.Button.withInline
                    |> Components.Button.view colorScheme
                )
            ]

        else
            [ Components.Button.new
                { onPress = Just Cancelled
                , label = text "Sitzung abbrechen"
                }
                |> Components.Button.view colorScheme
            ]
