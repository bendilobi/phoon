module Layouts.BaseLayout.SessionControls exposing (Model, Msg(..), Props, layout, map, sessionHintsID)

import Date
import Delay
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Layout exposing (Layout)
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.PageFading as Fading exposing (FadeState, Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Session as Session
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
    , multitouchEffects : List (Effect Msg)
    , singleTapEffects : List (Effect Msg)
    , sessionHints : Element contentMsg
    , nudgeSessionHints : Bool
    }


layout : Props contentMsg -> Shared.Model -> Route () -> Layout (Layouts.BaseLayout.Props contentMsg) Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init props shared
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
    , multitouchEffects = props.multitouchEffects
    , singleTapEffects = props.singleTapEffects
    , sessionHints = E.map fn props.sessionHints
    , nudgeSessionHints = props.nudgeSessionHints
    }



-- MODEL


type alias Model =
    { gesture : Swipe.Gesture
    , controlsShown : Bool
    , debounceBlock : Bool
    , fadeState : FadeState
    , doSessionEndFadeOut : Bool
    , swipeInitialPosition : Maybe Swipe.Position
    , swipeLocationX : Maybe Float
    , swipeLocationY : Maybe Float
    , swipeDirectionY : Maybe Bool
    , sessionHintsNudge : Float
    , sessionHintsNudgeToggle : Bool
    }


init : Props contentMsg -> Shared.Model -> () -> ( Model, Effect Msg )
init props shared _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
      , debounceBlock = False
      , fadeState = Fading.init shared.fadeIn
      , doSessionEndFadeOut = False
      , swipeInitialPosition = Nothing
      , swipeLocationX = Nothing
      , swipeLocationY = Nothing
      , swipeDirectionY = Nothing
      , sessionHintsNudge = 0
      , sessionHintsNudgeToggle = False
      }
    , Effect.batch
        [ Effect.setWakeLock
        , Effect.sendCmd <| Task.perform AdjustToday Date.today
        , if props.nudgeSessionHints then
            Effect.sendCmd <| Delay.after 3000 NudgeSessionHints

          else
            Effect.none
        , Effect.sendCmd <| Fading.initCmd shared.fadeIn ToggleFadeIn
        ]
    )


sessionHintsID : String
sessionHintsID =
    "sessionHints"



-- UPDATE


type Msg
    = SwipeStart Swipe.Event
    | Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | ReleaseDebounceBlock
    | AdjustToday Date.Date
    | ToggleFadeIn Fading.Trigger
    | SessionFadedOut
    | NudgeSessionHints
      -- To simulate gestures via buttons for debugging in desktop browser:
    | MouseNavTap
    | MouseNavSwipe


update : Props contentMsg -> Shared.Model -> Route () -> Msg -> Model -> ( Model, Effect Msg )
update props shared route msg model =
    case msg of
        SwipeStart event ->
            ( { model
                | gesture = Swipe.record event model.gesture
                , swipeInitialPosition = Just <| Swipe.locate event
              }
            , Effect.none
            )

        Swipe event ->
            let
                gesture =
                    Swipe.record event model.gesture

                isSingleFinger =
                    Swipe.maxFingers gesture == 1

                location =
                    Swipe.locate event
            in
            ( { model
                | gesture = gesture
                , swipeLocationX =
                    if isSingleFinger then
                        Just <| .x location

                    else
                        Nothing
                , swipeLocationY =
                    if isSingleFinger then
                        Just <| .y location

                    else
                        Nothing
                , swipeDirectionY =
                    case model.swipeDirectionY of
                        Nothing ->
                            case model.swipeInitialPosition of
                                Nothing ->
                                    model.swipeDirectionY

                                Just { x, y } ->
                                    if (location.x - x) < (location.y - y) then
                                        Just True

                                    else
                                        Just False

                        Just _ ->
                            model.swipeDirectionY
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
                    shared.deviceInfo.window.width * 0.9
            in
            ( { model
                | gesture = Swipe.blanco
                , swipeInitialPosition = Nothing
                , swipeLocationX = Nothing
                , swipeLocationY = Nothing
                , swipeDirectionY = Nothing
                , controlsShown = Swipe.isRightSwipe swipeSize gesture
                , debounceBlock = model.debounceBlock || multitouchRegistered
                , doSessionEndFadeOut = model.doSessionEndFadeOut || multitouchRegistered && route.path == Session.phasePath Session.End
              }
              -- TODO: Herausfinden, ob ich doch irgendwie ein sauberes "Mehr als 1 Finger beteiligt" hinkriege...
              --       Was aktuell zu passieren scheint: Beim Lupfen eines Fingers wird ein End Event
              --       ausgelöst. Wenn sich dann die zwei liegengebliebenen Finger kurz bewegen, gibts
              --       ein zweites End Event...
            , Effect.batch <|
                (if multitouchRegistered then
                    Effect.sendCmd <| Delay.after 1500 ReleaseDebounceBlock

                 else
                    Effect.none
                )
                    :: (if multitouchRegistered then
                            props.multitouchEffects

                        else
                            []
                       )
                    ++ (if singleTapRegistered then
                            props.singleTapEffects

                        else
                            []
                       )
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
                Effect.batch props.multitouchEffects

              else
                Effect.none
            )

        ToggleFadeIn fade ->
            ( { model | fadeState = Fading.update fade }
            , Effect.sendCmd <| Fading.updateCmd fade ToggleFadeIn
            )

        NudgeSessionHints ->
            let
                toggleSize =
                    15

                delay =
                    if model.sessionHintsNudge == toggleSize && model.sessionHintsNudgeToggle then
                        3000

                    else
                        200
            in
            ( { model
                | sessionHintsNudge =
                    if model.sessionHintsNudge == 0 then
                        toggleSize

                    else
                        0
                , sessionHintsNudgeToggle =
                    if model.sessionHintsNudge == 0 then
                        not model.sessionHintsNudgeToggle

                    else
                        model.sessionHintsNudgeToggle
              }
            , if props.nudgeSessionHints then
                Effect.sendCmd <| Delay.after delay NudgeSessionHints

              else
                Effect.none
            )


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

                    {- To compensate for the header which is "inFront": -}
                    , if shared.deviceInfo.orientation == Landscape then
                        paddingEach { top = sessionHeader.height, left = 0, right = 0, bottom = 0 }

                      else
                        padding 0
                    , inFront <| viewHeaderAndTouchOverlay props shared model toContentMsg
                    ]
                <|
                    el
                        [ width fill
                        , height fill
                        , paddingEach <| SafeArea.paddingEach shared.safeAreaInset
                        ]
                    <|
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
                    if model.swipeDirectionY == Just False then
                        case ( model.swipeInitialPosition, model.swipeLocationX ) of
                            ( Just initialPos, Just currentX ) ->
                                currentX - initialPos.x

                            ( _, _ ) ->
                                0

                    else
                        0
            in
            if model.controlsShown then
                0

            else
                shared.deviceInfo.window.width - dragDistance
        , htmlAttribute <|
            case model.swipeInitialPosition of
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


sessionHeader :
    { border : Int
    , padTop : Int
    , padBot : Int
    , pillHeight : Int
    , height : Int
    }
sessionHeader =
    let
        border =
            1

        padTop =
            5

        padBot =
            15

        pillHeight =
            14
    in
    { border = border
    , padTop = padTop
    , padBot = padBot
    , pillHeight = pillHeight
    , height = border + padTop + padBot + pillHeight
    }


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
                    , height <| px sessionHeader.pillHeight
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
    column
        [ width fill
        , height fill
        , behindContent <| viewSessionHints props shared model
        ]
        [ el
            ([ width fill
             , height <| px sessionHeader.height
             , Border.widthEach { bottom = sessionHeader.border, top = 0, left = 0, right = 0 }
             ]
                ++ CS.primary
            )
          <|
            (row
                [ width fill
                , height fill
                , paddingEach { left = 30, right = 15, top = sessionHeader.padTop, bottom = sessionHeader.padBot }
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


viewSessionHints : Props contentMsg -> Shared.Model -> Model -> Element contentMsg
viewSessionHints props shared model =
    let
        hintsHeight =
            case shared.sessionHintsHeight of
                Nothing ->
                    200

                Just height ->
                    height

        dragDistance =
            if model.swipeDirectionY == Just True then
                case ( model.swipeInitialPosition, model.swipeLocationY ) of
                    ( Just initialPos, Just currentY ) ->
                        currentY - initialPos.y

                    ( _, _ ) ->
                        0

            else
                0
    in
    el
        [ width fill
        , above <|
            el
                ([ width fill
                 , htmlAttribute <| Html.Attributes.id sessionHintsID
                 , moveDown <|
                    toFloat sessionHeader.height
                        + min hintsHeight (max dragDistance 0)
                        + (if props.nudgeSessionHints && dragDistance == 0 then
                            model.sessionHintsNudge

                           else
                            0
                          )
                 , htmlAttribute <|
                    case model.swipeInitialPosition of
                        Nothing ->
                            Transition.properties [ Transition.transform 500 [ Transition.easeOutExpo ] ]

                        _ ->
                            Html.Attributes.hidden False
                 , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 80, bottomRight = 80 }
                 , padding 30
                 , below <|
                    el [ centerX, Font.color CS.primaryColors.primary ] <|
                        (FeatherIcons.chevronDown
                            |> FeatherIcons.withSize 40
                            |> FeatherIcons.withStrokeWidth 1
                            |> FeatherIcons.toHtml []
                            |> html
                        )
                 ]
                    ++ CS.primary
                )
            <|
                el
                    [ width fill
                    , alignBottom
                    ]
                <|
                    props.sessionHints
        ]
        none
