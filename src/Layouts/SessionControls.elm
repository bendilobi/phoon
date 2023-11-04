module Layouts.SessionControls exposing (Model, Msg, Props, layout, map)

import Browser.Navigation
import Date
import Delay
import Effect exposing (Effect)
import Element as E exposing (..)
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


type alias Props contentMsg =
    { showCurrentCycle : Maybe Int
    , controlsTop : List (Element contentMsg)
    , controlsBottom : List (Element contentMsg)
    }


layout : Props contentMsg -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update props shared route
        , view = view props shared route
        , subscriptions = subscriptions
        }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { showCurrentCycle = props.showCurrentCycle
    , controlsTop = List.map (E.map fn) props.controlsTop
    , controlsBottom = List.map (E.map fn) props.controlsBottom
    }



-- MODEL


type alias Model =
    { gesture : Swipe.Gesture
    , controlsShown : Bool
    , debounceBlock : Bool
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
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
    | ReleaseDebounceBlock
    | AdjustToday Date.Date
      -- To simulate gestures via buttons for debugging in desktop browser:
    | MouseNavTap
    | MouseNavSwipe


update : Props contentMsg -> Shared.Model -> Route () -> Msg -> Model -> ( Model, Effect Msg )
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

        MouseNavSwipe ->
            ( { model | controlsShown = True }, Effect.none )

        MouseNavTap ->
            ( { model | controlsShown = False }
            , if not model.controlsShown then
                Effect.batch <| multitouchEffects shared route

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


view : Props contentMsg -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff Session"
    , attributes = content.attributes
    , element =
        el
            [ width fill
            , height fill
            , inFront <|
                column
                    [ width fill
                    , height fill
                    ]
                    [ if model.controlsShown && List.length props.controlsTop > 0 then
                        column
                            [ width fill
                            , padding 50
                            , behindContent <| el ([ alpha 0.6, width fill, height fill ] ++ CS.primary) none
                            ]
                        <|
                            props.controlsTop

                      else
                        none
                    , viewTouchOverlay Shared.showDebugButtons
                        |> E.map toContentMsg
                    , if model.controlsShown then
                        column
                            [ width fill
                            , padding 50

                            -- , paddingEach { bottom = 100, top = 50, left = 50, right = 50 }
                            , behindContent <| el ([ alpha 0.6, width fill, height fill ] ++ CS.primary) none
                            , spacing 30
                            ]
                            props.controlsBottom

                      else
                        none
                    ]
            ]
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
