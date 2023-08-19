module Layouts.SessionControls exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Element.Input exposing (button)
import Layout exposing (Layout)
import Lib.BreathingSession as BreathingSession
import Lib.Swipe as Swipe
import Lib.Tools as Tools
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias Props =
    { showSessionProgress : Bool }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update shared route
        , view = view props shared
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { gesture : Swipe.Gesture
    , controlsShown : Bool
    , debugButtonsShown : Bool
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
      , debugButtonsShown = False
      }
    , Effect.setWakeLock
    )



-- UPDATE


type Msg
    = Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | Cancelled
      -- To simulate gestures via buttons for debugging in desktop browser
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
            in
            ( { model
                | gesture = Swipe.blanco
                , controlsShown = Swipe.isRightSwipe 300 gesture
              }
            , if not model.controlsShown && Swipe.isTap gesture then
                Tools.navigateNext shared.session

              else
                Effect.none
            )

        Cancelled ->
            ( model, Tools.navigate Route.Path.Phases_SessionEnd )

        MouseNavSwipe ->
            ( { model | controlsShown = True }, Effect.none )

        MouseNavTap ->
            ( { model | controlsShown = False }
            , if not model.controlsShown then
                Tools.navigateNext shared.session

              else
                Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Props -> Shared.Model -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
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
                                viewSessionControls
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
                        [ centerX
                        , padding 10
                        ]
                    <|
                        text <|
                            "Runde "
                                ++ (String.fromInt <| BreathingSession.currentCycle shared.session)

                  else
                    none
                , el [ centerX, centerY ] content.element
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
        , padding 10
        , width fill
        , Font.center
        , Font.size 10
        ]
        { onPress = Just msg
        , label = text label
        }


viewSessionControls : Element Msg
viewSessionControls =
    column [ centerX, centerY ]
        [ button
            [ padding 20
            , BG.color <| rgb255 33 33 33
            ]
            { onPress = Just Cancelled
            , label = text "Sitzung abbrechen"
            }
        , el [ height <| px 100 ] none
        ]
