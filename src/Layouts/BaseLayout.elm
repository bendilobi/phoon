module Layouts.BaseLayout exposing (Model, Msg, Overlay(..), Props, layout, map)

import Browser.Dom
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode
import Layout exposing (Layout)
import Lib.ColorScheme as CS
import Lib.SafeArea as SafeArea
import Lib.Swipe as Swipe
import Lib.Texts as Texts
import Route exposing (Route)
import Shared
import Shared.Model
import Simple.Transition as Transition
import Task
import Time
import View exposing (View)


type alias Props contentMsg =
    { overlay : Overlay contentMsg }


type Overlay contentMsg
    = NoOverlay
    | ModalDialog (Element contentMsg)
    | InfoWindow
        { header : String
        , info : Element contentMsg
        , onClose : contentMsg
        }


layout : Props contentMsg -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update shared
        , view = view props shared
        , subscriptions = subscriptions
        }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { overlay =
        case props.overlay of
            NoOverlay ->
                NoOverlay

            ModalDialog lmnt ->
                ModalDialog <| E.map fn lmnt

            InfoWindow { header, info, onClose } ->
                InfoWindow
                    { header = header
                    , info = E.map fn info
                    , onClose = fn onClose
                    }
    }



-- MODEL


type alias Model =
    { swipeGesture : Swipe.Gesture
    , swipeInitialY : Maybe Float
    , swipeLocationY : Maybe Float
    , infoContentViewportAtTop : Bool
    , swiping : Bool
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { swipeGesture = Swipe.blanco
      , swipeInitialY = Nothing
      , swipeLocationY = Nothing
      , infoContentViewportAtTop = True
      , swiping = False
      }
    , Effect.none
    )


infoContentID =
    "infocontent"



-- UPDATE


type Msg
    = OnInfoWindowResize
    | SwipeStart Swipe.Event
    | TimedSwipeStart Swipe.Event Time.Posix
    | SwipeMove Swipe.Event
    | TimedSwipeMove Swipe.Event Time.Posix
    | SwipeEnd Swipe.Event
    | TimedSwipeEnd Swipe.Event Time.Posix
    | SwipeCancel Swipe.Event
    | PointerDetected Bool
    | OnScrollEnd
    | ReceivedInfoContentViewport (Result Browser.Dom.Error Browser.Dom.Viewport)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        OnInfoWindowResize ->
            ( model
            , case shared.infoWindowState of
                Shared.Model.Half ->
                    Effect.setInfoWindowState Shared.Model.Max

                Shared.Model.Max ->
                    Effect.setInfoWindowState Shared.Model.Half

                Shared.Model.Closed ->
                    Effect.none
            )

        SwipeStart event ->
            ( model
            , Effect.sendCmd <| Task.perform (TimedSwipeStart event) Time.now
            )

        TimedSwipeStart event time ->
            ( if model.infoContentViewportAtTop then
                { model
                    | swipeGesture = Swipe.recordWithTime event time model.swipeGesture
                    , swipeInitialY = Just <| .y <| Swipe.locate event
                    , swiping = True
                }

              else
                {- Only register swiping when the info content is scrolled to the top.
                   This is needed because iOS doesn't yet support the "scrollend" event - we
                   use "scroll" instead. But since this fires continuously, swiping would be
                   triggered as soon as the content viewport is at the top, during scrolling.
                   We only want to react to swiping if the content was at the top at the beginning.
                -}
                { model | swiping = False }
            , Effect.none
            )

        SwipeMove event ->
            ( model
            , Effect.sendCmd <| Task.perform (TimedSwipeMove event) Time.now
            )

        TimedSwipeMove event time ->
            ( { model
                | swipeGesture = Swipe.recordWithTime event time model.swipeGesture
                , swipeLocationY = Swipe.locate event |> .y |> Just
              }
            , Effect.none
            )

        SwipeEnd event ->
            ( model
            , Effect.sendCmd <| Task.perform (TimedSwipeEnd event) Time.now
            )

        TimedSwipeEnd event time ->
            let
                gesture =
                    Swipe.recordWithTime event time model.swipeGesture

                swipeThreshold =
                    shared.deviceInfo.window.height / 5

                bigSwipeThreshold =
                    shared.deviceInfo.window.height - swipeThreshold

                switchDown =
                    model.infoContentViewportAtTop
                        && (Swipe.isDownSwipe swipeThreshold gesture
                                || Swipe.isDownFlick gesture
                           )

                switchCompletelyDown =
                    model.infoContentViewportAtTop && Swipe.isDownSwipe bigSwipeThreshold gesture

                switchUp =
                    Swipe.isUpSwipe swipeThreshold gesture || Swipe.isUpFlick gesture
            in
            ( { model
                | swipeGesture = Swipe.blanco
                , swipeInitialY = Nothing
                , swipeLocationY = Nothing
                , swiping = False
              }
            , if switchCompletelyDown then
                case shared.infoWindowState of
                    Shared.Model.Max ->
                        Effect.setInfoWindowState Shared.Model.Closed

                    _ ->
                        Effect.none

              else if switchDown then
                case shared.infoWindowState of
                    Shared.Model.Max ->
                        Effect.setInfoWindowState Shared.Model.Half

                    Shared.Model.Half ->
                        Effect.setInfoWindowState Shared.Model.Closed

                    Shared.Model.Closed ->
                        Effect.none

              else if switchUp then
                case shared.infoWindowState of
                    Shared.Model.Max ->
                        Effect.none

                    Shared.Model.Half ->
                        Effect.setInfoWindowState Shared.Model.Max

                    Shared.Model.Closed ->
                        Effect.none

              else
                Effect.none
            )

        SwipeCancel _ ->
            ( { model
                | swipeGesture = Swipe.blanco
                , swipeInitialY = Nothing
                , swipeLocationY = Nothing
                , swiping = False
              }
            , Effect.none
            )

        PointerDetected isMouse ->
            ( model
            , Effect.pointerDetected isMouse
            )

        OnScrollEnd ->
            ( model
            , Effect.sendCmd <| Task.attempt ReceivedInfoContentViewport <| Browser.Dom.getViewportOf infoContentID
            )

        ReceivedInfoContentViewport (Ok { viewport }) ->
            ( { model | infoContentViewportAtTop = viewport.y <= 0 }, Effect.none )

        ReceivedInfoContentViewport (Err _) ->
            ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Props contentMsg -> Shared.Model -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared { toContentMsg, model, content } =
    { title = content.title ++ " | " ++ Texts.appName
    , attributes = []
    , element =
        el
            (CS.primary
                ++ [ width fill
                   , height fill
                   , clip
                   , below <| viewInfoWindow props shared model toContentMsg
                   ]
                ++ (case shared.mouseDetected of
                        Nothing ->
                            let
                                mouseDetector =
                                    (\e -> e.pointerType == Pointer.MouseType)
                                        >> (\b -> toContentMsg <| PointerDetected b)
                            in
                            [ htmlAttribute <| Pointer.onMove mouseDetector
                            , htmlAttribute <| Pointer.onDown mouseDetector
                            ]

                        _ ->
                            []
                   )
            )
        <|
            el
                [ width fill
                , height fill
                , inFront <|
                    {- This is the semi-transparent overlay that covers the screen if a dialog or an info window is shown.
                       CSS transitions are used to make it appear and disappear smoothly without having to manage state.
                       To this behalf, it always exists and is moved out of the screen in case it isn't needed.
                    -}
                    el
                        ([ width fill
                         , height fill
                         , inFront <|
                            case props.overlay of
                                ModalDialog dialog ->
                                    dialog

                                _ ->
                                    none
                         ]
                            ++ (case props.overlay of
                                    NoOverlay ->
                                        [ moveUp shared.deviceInfo.window.height

                                        {- Here we basically slow down the disappearance of the overlay so that the user
                                           is able to see the opacity transition:
                                        -}
                                        , htmlAttribute <| Transition.properties [ Transition.transform 0 [ Transition.delay 200 ] ]
                                        ]

                                    _ ->
                                        [ moveUp 0 ]
                               )
                        )
                    <|
                        el
                            ([ width fill
                             , height fill
                             , BG.color <| rgb 0 0 0
                             , alpha <|
                                case props.overlay of
                                    ModalDialog _ ->
                                        0.3

                                    InfoWindow _ ->
                                        0.1

                                    NoOverlay ->
                                        0
                             , htmlAttribute <| Transition.properties [ Transition.opacity 200 [ Transition.easeOut ] ]
                             ]
                                ++ (case props.overlay of
                                        InfoWindow { onClose } ->
                                            [ Events.onClick onClose ]

                                        _ ->
                                            []
                                   )
                            )
                            none
                ]
            <|
                content.element
    }


viewInfoWindow : Props contentMsg -> Shared.Model -> Model -> (Msg -> contentMsg) -> Element contentMsg
viewInfoWindow props shared model toContentMsg =
    let
        topGap =
            31

        maxHeight =
            shared.deviceInfo.window.height - topGap

        halfHeight =
            shared.deviceInfo.window.height / 2 + 52

        safeAreaBottom =
            --TODO: Offenbar ist der Wert hin und wieder fälschlicherweise 0...
            --      => in diesem Fall einen Default setzen?
            SafeArea.paddingEach shared.safeAreaInset
                |> .bottom
                |> toFloat
    in
    el
        [ width <| px <| (shared.deviceInfo.window.width |> round) - (SafeArea.maxX shared.safeAreaInset * 2)
        , height <| px <| round <| shared.deviceInfo.window.height - topGap
        , centerX
        , Border.roundEach
            { topLeft = 25
            , topRight = 25
            , bottomLeft = 0
            , bottomRight = 0
            }
        , clip
        , moveUp <|
            case props.overlay of
                InfoWindow _ ->
                    let
                        dragDistance =
                            case ( model.swipeInitialY, model.swipeLocationY ) of
                                ( Just initialY, Just currentY ) ->
                                    currentY - initialY

                                ( _, _ ) ->
                                    0
                    in
                    (case shared.infoWindowState of
                        Shared.Model.Max ->
                            if model.infoContentViewportAtTop then
                                maxHeight - dragDistance

                            else
                                maxHeight

                        Shared.Model.Half ->
                            halfHeight - dragDistance

                        Shared.Model.Closed ->
                            0
                    )
                        |> min maxHeight
                        |> max 0

                _ ->
                    0
        , BG.color <| rgba255 241 241 230 0.55

        {- backdrop-filter needs a "-webkit-" prefix in iOS prior to 18, so we try to respect that here: -}
        , htmlAttribute <|
            case shared.iOSVersion of
                Nothing ->
                    Html.Attributes.attribute "style" "backdrop-filter: blur(20px);"

                Just v ->
                    if v < 18 then
                        Html.Attributes.attribute "style" "-webkit-backdrop-filter: blur(20px);"

                    else
                        Html.Attributes.attribute "style" "backdrop-filter: blur(20px);"
        , htmlAttribute <|
            case model.swipeLocationY of
                {- Suppress animation while swiping -}
                Nothing ->
                    Transition.properties [ Transition.transform 500 [ Transition.easeOutExpo ] ]

                _ ->
                    Html.Attributes.hidden False
        , inFront <|
            el
                [ width fill
                ]
            <|
                {- Resize-button -}
                Input.button
                    [ centerX
                    , height <| px 25
                    , width fill
                    , padding 5
                    ]
                    { onPress = Just <| toContentMsg OnInfoWindowResize
                    , label =
                        el
                            [ height <| px 5
                            , width <| px 40
                            , BG.color <| CS.greyOverTransparencyColor shared.colorScheme
                            , Border.rounded 4
                            , alignTop
                            ]
                            none
                    }
        , inFront <|
            el
                []
            <|
                {- Close button -}
                Input.button
                    [ Font.color <| CS.greyOverTransparencyColor shared.colorScheme
                    ]
                    { onPress =
                        case props.overlay of
                            InfoWindow { onClose } ->
                                Just onClose

                            _ ->
                                Nothing
                    , label =
                        el
                            [ padding 15
                            ]
                        <|
                            (FeatherIcons.withSize 32 FeatherIcons.x
                                |> FeatherIcons.withStrokeWidth 1.3
                                |> FeatherIcons.toHtml []
                                |> html
                            )
                    }
        ]
    <|
        column
            ([ width fill
             , height fill
             , Font.color <| CS.primaryColors.primary
             , Font.size 14

             --  , paddingEach { top = 0, left = 0, right = 0, bottom = 20 } --safeAreaBottom |> round }
             , htmlAttribute <| Swipe.onStart <| \e -> toContentMsg <| SwipeStart e
             ]
                ++ (if model.swiping then
                        [ htmlAttribute <| Swipe.onMove <| \e -> toContentMsg <| SwipeMove e
                        , htmlAttribute <| Swipe.onEndWithOptions { stopPropagation = False, preventDefault = False } <| \e -> toContentMsg <| SwipeEnd e
                        , htmlAttribute <| Swipe.onCancel <| \e -> toContentMsg <| SwipeCancel e
                        ]

                    else
                        []
                   )
            )
            [ el
                {- Header -}
                [ width fill
                , Font.bold
                , Font.center
                , Font.size 18

                -- , paddingEach { left = 30, top = 22, right = 30, bottom = 20 }
                , paddingEach { left = 53, top = 22, right = 53, bottom = 20 }
                ]
              <|
                case props.overlay of
                    InfoWindow { header } ->
                        paragraph [] [ text header ]

                    _ ->
                        none
            , {- Information content -}
              case props.overlay of
                InfoWindow { info } ->
                    el
                        ([ width fill
                         , height fill

                         --  , paddingXY 23 0
                         , paddingEach { top = 0, left = 23, right = 23, bottom = 23 }
                         ]
                            ++ (case shared.infoWindowState of
                                    Shared.Model.Max ->
                                        [ scrollbarY
                                        , htmlAttribute <| Html.Attributes.style "min-height" "auto"
                                        , htmlAttribute <| Html.Attributes.id infoContentID
                                        , htmlAttribute <|
                                            if shared.iOSVersion == Nothing then
                                                {- Doesn't work on Safari, see https://bugs.webkit.org/show_bug.cgi?id=201556 -}
                                                Html.Events.on "scrollend" <| Json.Decode.succeed <| toContentMsg OnScrollEnd

                                            else
                                                Html.Events.on "scroll" <| Json.Decode.succeed <| toContentMsg OnScrollEnd
                                        ]

                                    _ ->
                                        []
                               )
                        )
                    <|
                        -- case shared.infoWindowState of
                        -- Shared.Model.Max ->
                        --     el
                        --         [ width fill
                        --         , height fill
                        --         , scrollbarY
                        --         , htmlAttribute <| Html.Attributes.style "min-height" "auto"
                        --         , htmlAttribute <| Html.Attributes.id infoContentID
                        --         , htmlAttribute <|
                        --             if shared.iOSVersion == Nothing then
                        --                 {- Doesn't work on Safari, see https://bugs.webkit.org/show_bug.cgi?id=201556 -}
                        --                 Html.Events.on "scrollend" <| Json.Decode.succeed <| toContentMsg OnScrollEnd
                        --             else
                        --                 Html.Events.on "scroll" <| Json.Decode.succeed <| toContentMsg OnScrollEnd
                        --         ]
                        --     <|
                        --         info
                        -- _ ->
                        info

                _ ->
                    none

            -- , let
            --     { top, bottom, left, right } =
            --         SafeArea.paddingEach shared.safeAreaInset
            --   in
            --   --   paragraph [ paddingXY 0 20, Font.size 15 ]
            --   --     [ el [ Font.bold ] <| text "Safe Area: "
            --   --     , text "top: "
            --   --     , text <| String.fromInt top
            --   --     , text ", bottom : "
            --   --     , text <| String.fromInt bottom
            --   --     , text ", left: "
            --   --     , text <| String.fromInt left
            --   --     , text ", right: "
            --   --     , text <| String.fromInt right
            --   --     ]
            --   el [ width fill, paddingXY 0 30 ] <| el [ alignRight ] <| text <| String.fromInt bottom
            -- , let
            --     { bottom } =
            --         SafeArea.paddingEach shared.safeAreaInset
            --   in
            --   paragraph [ Font.size 11, paddingXY 20 30 ]
            --     [ text "SwipeInitialY: "
            --     , text <| String.fromFloat <| Maybe.withDefault 0 <| model.swipeInitialY
            --     , text ", CurrentY: "
            --     , text <| String.fromFloat <| Maybe.withDefault 0 <| model.swipeLocationY
            --     , text ", sab: "
            --     , text <| String.fromInt bottom
            --     ]
            ]
