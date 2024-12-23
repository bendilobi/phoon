module Layouts.BaseLayout.SessionControls exposing (Model, Msg(..), Props, SessionHints, layout, map)

import Browser.Events
import Delay
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Json.Decode
import Key
import Layout exposing (Layout)
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.PageFading as Fading exposing (FadeState, Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Session as Session
import Lib.Swipe as Swipe
import Lib.Texts as Texts
import Route exposing (Route)
import Shared
import Simple.Transition as Transition
import View exposing (View)


type alias Props contentMsg =
    { currentCycle : Int
    , controlsTop : List (Element contentMsg)
    , controlsBottom : List (Element contentMsg)
    , fadeOut : Fading.Trigger
    , overlay : Layouts.BaseLayout.Overlay contentMsg
    , goNextEffects : List (Effect Msg)
    , pageActionEffects : List (Effect Msg)
    , sessionHints : SessionHints contentMsg
    , nudgeSessionHints : Bool
    }


type alias SessionHints contentMsg =
    { heading : String, content : Element contentMsg }


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

            Layouts.BaseLayout.InfoWindow { header, info } ->
                Layouts.BaseLayout.InfoWindow
                    { header = header
                    , info = E.map fn info
                    }
    , goNextEffects = props.goNextEffects
    , pageActionEffects = props.pageActionEffects
    , sessionHints = { heading = props.sessionHints.heading, content = E.map fn props.sessionHints.content }
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
    , hintsShownByKeyPress : Bool
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
      , hintsShownByKeyPress = shared.pointerIsMouse == Just True
      }
    , Effect.batch
        [ Effect.setWakeLock
        , Effect.adjustToday
        , if props.nudgeSessionHints then
            Effect.sendCmd <| Delay.after 2000 NudgeSessionHints

          else
            Effect.none
        , Effect.sendCmd <| Fading.initCmd shared.fadeIn ToggleFadeIn
        ]
    )



-- UPDATE


type Msg
    = SwipeStart Swipe.Event
    | Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | ReleaseDebounceBlock
    | ToggleFadeIn Fading.Trigger
    | SessionFadedOut
    | NudgeSessionHints
    | KeyUp Key.Key
    | KeyDown Key.Key
    | GoNextTriggered
    | PageActionTriggered


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
              }
            , if Swipe.maxFingers gesture == 3 then
                Effect.sendMsg GoNextTriggered

              else if Swipe.isTap gesture && Swipe.maxFingers gesture == 1 then
                Effect.sendMsg PageActionTriggered

              else
                Effect.none
            )

        KeyDown key ->
            case key of
                Key.ArrowDown ->
                    ( { model | hintsShownByKeyPress = not model.controlsShown }, Effect.none )

                _ ->
                    ( model, Effect.none )

        KeyUp key ->
            case key of
                Key.Space ->
                    ( model, Effect.sendMsg GoNextTriggered )

                Key.Escape ->
                    ( { model
                        | controlsShown = not model.controlsShown
                        , hintsShownByKeyPress = False
                      }
                    , Effect.none
                    )

                Key.ArrowDown ->
                    ( { model
                        | controlsShown = False
                        , hintsShownByKeyPress = False
                      }
                    , Effect.none
                    )

                Key.Enter ->
                    ( model, Effect.sendMsg PageActionTriggered )

                _ ->
                    ( { model
                        | controlsShown = False
                        , hintsShownByKeyPress = False
                      }
                    , Effect.none
                    )

        GoNextTriggered ->
            let
                goNextAllowed =
                    not model.controlsShown
                        && not model.debounceBlock
            in
            ( { model
                | controlsShown = False
                , debounceBlock = model.debounceBlock || goNextAllowed
                , doSessionEndFadeOut = model.doSessionEndFadeOut || goNextAllowed && route.path == Session.phasePath Session.End
              }
            , if goNextAllowed then
                Effect.batch
                    ((Effect.sendCmd <| Delay.after 1500 ReleaseDebounceBlock)
                        :: props.goNextEffects
                    )

              else
                Effect.none
            )

        PageActionTriggered ->
            ( { model | controlsShown = False }, Effect.batch props.pageActionEffects )

        SessionFadedOut ->
            ( model
            , Effect.batch
                [ Effect.playSound Session.EndSound
                , Effect.sessionEnded Session.Finished
                ]
            )

        ReleaseDebounceBlock ->
            ( { model | debounceBlock = False }, Effect.none )

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
                        2000

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
    Sub.batch
        [ Browser.Events.onKeyUp (Json.Decode.map KeyUp Key.decoder)
        , Browser.Events.onKeyDown (Json.Decode.map KeyDown Key.decoder)
        ]



-- VIEW


view : Props contentMsg -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title ++ " | " ++ Texts.appName ++ " Session"
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
                        ([ width fill
                         , height fill
                         , paddingEach <| SafeArea.paddingEach shared.safeAreaInset
                         ]
                            ++ content.attributes
                        )
                    <|
                        content.element
    }


viewTouchOverlay : Element Msg
viewTouchOverlay =
    el
        [ width fill
        , height fill
        , htmlAttribute <| Swipe.onStart SwipeStart
        , htmlAttribute <| Swipe.onMove Swipe
        , htmlAttribute <| Swipe.onEnd SwipeEnd
        ]
    <|
        none


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
        , viewTouchOverlay
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

        { left, right } =
            SafeArea.paddingEach shared.safeAreaInset
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
                , paddingEach { left = 30 + left, right = 15 + right, top = sessionHeader.padTop, bottom = sessionHeader.padBot }
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
            viewTouchOverlay
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

        { left, right } =
            SafeArea.paddingEach shared.safeAreaInset
    in
    el
        [ width fill
        , above <|
            el
                ([ width fill
                 , htmlAttribute <| Html.Attributes.id Shared.sessionHintsID
                 , moveDown <|
                    if model.hintsShownByKeyPress then
                        hintsHeight

                    else
                        toFloat sessionHeader.height
                            + min hintsHeight (max dragDistance 0)
                            + (if props.nudgeSessionHints && model.swipeInitialPosition == Nothing then
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
                 , paddingEach { top = 15, left = 30 + left, right = 30 + right, bottom = 30 }
                 , below <|
                    el [ centerX, Font.color CS.primaryColors.primary ] <|
                        (FeatherIcons.chevronDown
                            |> FeatherIcons.withSize 40
                            |> FeatherIcons.withStrokeWidth 1.5
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
                    column
                        [ spacing 20
                        , Font.size 15
                        , centerX
                        ]
                        [ paragraph [ Font.center, Font.bold ] [ text props.sessionHints.heading ]
                        , props.sessionHints.content
                        ]
        ]
        none
