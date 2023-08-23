module Layouts.SessionControls exposing (Model, Msg, Props, layout)

import Components.Button
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Layout exposing (Layout)
import Lib.BreathingSession as BS
import Lib.Swipe as Swipe
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
    , Effect.setWakeLock
    )



-- UPDATE


type Msg
    = Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | Cancelled
    | AddCycle
    | ReleaseDebounceBlock
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

              else
                Effect.none
            )

        Cancelled ->
            ( model
            , if route.path == Route.Path.Phases_SessionStart then
                Effect.navigate Route.Path.Session

              else
                Effect.navigate Route.Path.Phases_SessionEnd
            )

        AddCycle ->
            let
                newSession =
                    BS.addCycle shared.session
            in
            ( { model | controlsShown = False }
            , Effect.batch
                [ Effect.sessionUpdated newSession
                , Effect.navigate <| BS.currentPath newSession
                ]
            )

        ReleaseDebounceBlock ->
            ( { model | debounceBlock = False }, Effect.none )

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
                                viewSessionControls route
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
                        [ width fill
                        , BG.color <| rgb255 50 49 46
                        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                        , Border.color <| rgb255 34 33 31
                        ]
                    <|
                        el
                            [ centerX
                            , padding 10
                            , Font.size 30
                            , Font.color <| rgb 1 1 1
                            ]
                        <|
                            text <|
                                "Runde "
                                    ++ (String.fromInt <| BS.currentCycle shared.session)

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


viewSessionControls : Route () -> Element Msg
viewSessionControls route =
    column [ centerX, centerY ]
        [ Components.Button.view
          -- [ padding 20
          -- , BG.color <| rgb255 33 33 33
          -- ]
          <|
            if route.path == Route.Path.Phases_SessionEnd then
                { onPress = Just AddCycle
                , label = text "Noch 'ne Runde"
                }

            else
                { onPress = Just Cancelled
                , label = text "Sitzung abbrechen"
                }
        , el [ height <| px 100 ] none
        ]
