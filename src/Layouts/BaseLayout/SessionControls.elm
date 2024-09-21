module Layouts.BaseLayout.SessionControls exposing (Model, Msg, Props, layout, map)

import Date
import Delay
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Layout exposing (Layout)
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.PageFading as Fading exposing (FadeState, Trigger(..))
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Swipe as Swipe
import Route exposing (Route)
import Shared
import Simple.Transition as Transition
import Task
import View exposing (View)


type alias Props contentMsg =
    { currentCycle : Int
    , controlsTop : List (Element contentMsg)
    , controlsBottom : List (Element contentMsg)
    , fadeOut : Fading.Trigger
    , overlay : Layouts.BaseLayout.Overlay contentMsg

    --TODO: Doch hier Callbacks für die Touch-Events übergeben
    }


layout : Props contentMsg -> Shared.Model -> Route () -> Layout (Layouts.BaseLayout.Props contentMsg) Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init shared
        , update = update props shared route
        , view = view props shared route
        , subscriptions = subscriptions
        }
        |> Layout.withParentProps { overlay = props.overlay }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { currentCycle = props.currentCycle
    , controlsTop = List.map (E.map fn) props.controlsTop
    , controlsBottom = List.map (E.map fn) props.controlsBottom
    , fadeOut = props.fadeOut
    , overlay =
        case props.overlay of
            Layouts.BaseLayout.NoOverlay ->
                Layouts.BaseLayout.NoOverlay

            Layouts.BaseLayout.ModalDialog lmnt ->
                Layouts.BaseLayout.ModalDialog <| E.map fn lmnt

            Layouts.BaseLayout.InfoWindow { header, info, onClose } ->
                Layouts.BaseLayout.InfoWindow
                    { header = header
                    , info = E.map fn info
                    , onClose = fn onClose
                    }
    }



-- MODEL


type alias Model =
    { gesture : Swipe.Gesture
    , controlsShown : Bool
    , debounceBlock : Bool
    , fadeState : FadeState
    , doSessionEndFadeOut : Bool
    , swipeInitialX : Maybe Float
    , swipeLocationX : Maybe Float
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
      , debounceBlock = False
      , fadeState = Fading.init shared.fadeIn
      , doSessionEndFadeOut = False
      , swipeInitialX = Nothing
      , swipeLocationX = Nothing
      }
    , Effect.batch
        [ Effect.setWakeLock
        , Effect.sendCmd <| Task.perform AdjustToday Date.today
        , Effect.sendCmd <| Fading.initCmd shared.fadeIn ToggleFadeIn
        ]
    )



-- UPDATE


type Msg
    = SwipeStart Swipe.Event
    | Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | ReleaseDebounceBlock
    | AdjustToday Date.Date
    | ToggleFadeIn Fading.Trigger
    | SessionFadedOut
      -- To simulate gestures via buttons for debugging in desktop browser:
    | MouseNavTap
    | MouseNavSwipe


update : Props contentMsg -> Shared.Model -> Route () -> Msg -> Model -> ( Model, Effect Msg )
update props shared route msg model =
    case msg of
        SwipeStart event ->
            ( { model
                | gesture = Swipe.record event model.gesture
                , swipeInitialX = Just <| .x <| Swipe.locate event
              }
            , Effect.none
            )

        Swipe event ->
            let
                gesture =
                    Swipe.record event model.gesture

                isSingleFinger =
                    Swipe.maxFingers gesture == 1
            in
            ( { model
                | gesture = gesture
                , swipeLocationX =
                    if isSingleFinger then
                        Just <| .x <| Swipe.locate event

                    else
                        Nothing
              }
            , Effect.none
            )

        SwipeEnd event ->
            let
                gesture =
                    Swipe.record event model.gesture

                multitouchRegistered =
                    not model.controlsShown
                        && not model.debounceBlock
                        && (Swipe.maxFingers gesture == 3)

                singleTapRegistered =
                    not model.controlsShown
                        && Swipe.isTap gesture
                        && (Swipe.maxFingers gesture == 1)

                swipeSize =
                    shared.deviceInfo.window.width * 0.8
            in
            ( { model
                | gesture = Swipe.blanco
                , swipeInitialX = Nothing
                , swipeLocationX = Nothing
                , controlsShown = Swipe.isRightSwipe swipeSize gesture
                , debounceBlock = model.debounceBlock || multitouchRegistered
                , doSessionEndFadeOut = model.doSessionEndFadeOut || multitouchRegistered && route.path == Session.phasePath Session.End
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

        SessionFadedOut ->
            ( model
            , Effect.batch
                [ Effect.playSound Session.EndSound
                , Effect.sessionEnded Session.Finished
                ]
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

        ToggleFadeIn fade ->
            ( { model | fadeState = Fading.update fade }
            , Effect.sendCmd <| Fading.updateCmd fade ToggleFadeIn
            )


multitouchEffects : Shared.Model -> Route () -> List (Effect Msg)
multitouchEffects shared route =
    [ if route.path == Session.phasePath Session.Retention then
        --- The user left the retention by multitouch, so we add the data
        Effect.resultsUpdated <| SessionResults.addRetention shared.results

      else if route.path == Session.phasePath Session.End then
        Effect.sendCmd <| Delay.after Fading.duration SessionFadedOut

      else
        Effect.none
    , Effect.navigateNext shared.session
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
                if model.doSessionEndFadeOut then
                    Fading.fadeOverlay (FadeWith Fading.sessionFadingColor) model.fadeState

                else
                    Fading.fadeOverlay props.fadeOut model.fadeState
            ]
        <|
            el
                [ width fill
                , height fill
                , inFront <| viewControls props shared model toContentMsg
                ]
            <|
                el
                    [ width fill
                    , height fill
                    , inFront <| viewHeaderAndTouchOverlay props shared model toContentMsg
                    ]
                    content.element
    }


viewTouchOverlay : Bool -> Element Msg
viewTouchOverlay debug =
    el
        [ width fill
        , height fill
        , htmlAttribute <| Swipe.onStart SwipeStart
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
    Input.button
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


viewControls : Props contentMsg -> Shared.Model -> Model -> (Msg -> contentMsg) -> Element contentMsg
viewControls props shared model toContentMsg =
    let
        borderRounded =
            if model.controlsShown then
                Border.rounded 0

            else
                Border.roundEach { topRight = 25, bottomRight = 25, topLeft = 0, bottomLeft = 0 }
    in
    column
        [ width fill
        , height fill
        , moveLeft <|
            let
                dragDistance =
                    case ( model.swipeInitialX, model.swipeLocationX ) of
                        ( Just initialX, Just currentX ) ->
                            currentX - initialX

                        ( _, _ ) ->
                            0
            in
            if model.controlsShown then
                0

            else
                shared.deviceInfo.window.width - dragDistance
        , htmlAttribute <|
            case model.swipeInitialX of
                Nothing ->
                    Transition.properties [ Transition.transform 500 [ Transition.easeOutExpo ] ]

                _ ->
                    Html.Attributes.hidden False
        ]
        [ if List.length props.controlsTop > 0 then
            column
                [ width fill
                , padding 50
                , behindContent <|
                    el
                        ([ alpha 0.6
                         , width fill
                         , height fill
                         , borderRounded
                         ]
                            ++ CS.primary
                        )
                        none
                ]
            <|
                props.controlsTop

          else
            none
        , viewTouchOverlay Shared.showDebugButtons
            |> E.map toContentMsg
        , column
            [ width fill
            , paddingEach { bottom = 100, top = 50, left = 50, right = 50 }
            , behindContent <|
                el
                    ([ alpha 0.6
                     , width fill
                     , height fill
                     , borderRounded
                     ]
                        ++ CS.primary
                    )
                    none
            , spacing 30
            ]
            props.controlsBottom
        ]


viewHeaderAndTouchOverlay : Props contentMsg -> Shared.Model -> Model -> (Msg -> contentMsg) -> Element contentMsg
viewHeaderAndTouchOverlay props shared model toContentMsg =
    let
        viewCycle : Bool -> Bool -> Element msg
        viewCycle completed final =
            let
                color =
                    if completed then
                        CS.primaryColors.font

                    else
                        rgb 0.3 0.3 0.3
            in
            row [ width fill, centerY ]
                [ el
                    [ width fill
                    , centerY
                    , height <| px 14
                    , Border.rounded 7
                    , BG.color color
                    ]
                    none
                , el
                    [ width <| px 15
                    , centerY
                    , height <| px 1
                    , BG.color color

                    {- The rightmost connector will be transparent and its width must be
                       subtracted from the right padding => all the cycle-symbols will be the
                       same size by using width fill.
                    -}
                    , transparent final
                    ]
                    none
                ]
    in
    column [ width fill, height fill ]
        [ el
            ([ width fill
             , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
             ]
                ++ CS.primary
            )
          <|
            (row
                [ width fill
                , height fill
                , paddingEach { left = 30, right = 15, top = 5, bottom = 15 }
                ]
             <|
                (List.map
                    (\cycleNr ->
                        viewCycle True
                            (Session.remainingCycles shared.session == 0 && cycleNr == props.currentCycle)
                    )
                 <|
                    List.range 1 props.currentCycle
                )
                    ++ (List.map (\cycleNr -> viewCycle False (cycleNr == Session.remainingCycles shared.session)) <|
                            List.range 1 <|
                                Session.remainingCycles shared.session
                       )
            )
        , if model.controlsShown then
            none

          else
            viewTouchOverlay Shared.showDebugButtons
                |> E.map toContentMsg
        ]
