module Layouts.BaseLayout exposing (Model, Msg, Overlay(..), Props, layout, map)

import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html.Attributes
import Layout exposing (Layout)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.SafeArea as SafeArea
import Lib.Swipe as Swipe
import Route exposing (Route)
import Shared
import Shared.Model
import Simple.Transition as Transition
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
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { swipeGesture = Swipe.blanco
      , swipeInitialY = Nothing
      , swipeLocationY = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = OnInfoWindowResize
    | SwipeStart Swipe.Event
    | Swipe Swipe.Event
    | SwipeEnd Swipe.Event


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

        SwipeStart touch ->
            ( { model
                | swipeGesture = Swipe.record touch model.swipeGesture
                , swipeInitialY = Just <| .y <| Swipe.locate touch
              }
            , Effect.none
            )

        Swipe touch ->
            ( { model
                | swipeGesture = Swipe.record touch model.swipeGesture
                , swipeLocationY = Swipe.locate touch |> .y |> Just
              }
            , Effect.none
            )

        SwipeEnd touch ->
            let
                gesture =
                    Swipe.record touch model.swipeGesture

                swipeThreshold =
                    shared.deviceInfo.window.height / 4

                bigSwipeThreshold =
                    shared.deviceInfo.window.height - swipeThreshold

                switchDown =
                    Swipe.isDownSwipe swipeThreshold gesture

                switchCompletelyDown =
                    Swipe.isDownSwipe bigSwipeThreshold gesture

                switchUp =
                    Swipe.isUpSwipe swipeThreshold gesture
            in
            ( { model
                | swipeGesture = Swipe.blanco
                , swipeInitialY = Nothing
                , swipeLocationY = Nothing
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Props contentMsg -> Shared.Model -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
    , attributes = []
    , element =
        el
            (content.attributes
                ++ [ width fill
                   , height fill
                   , clip
                   , below <| viewInfoWindow props shared model toContentMsg
                   ]
            )
        <|
            el
                [ width fill
                , height fill
                , inFront <|
                    case props.overlay of
                        ModalDialog dialog ->
                            el
                                [ width fill
                                , height fill
                                , inFront <| dialog
                                ]
                            <|
                                el
                                    [ width fill
                                    , height fill
                                    , alpha 0.3
                                    , BG.color <| rgb 0 0 0
                                    ]
                                    none

                        NoOverlay ->
                            none

                        InfoWindow { onClose } ->
                            case shared.infoWindowState of
                                Shared.Model.Closed ->
                                    none

                                _ ->
                                    el
                                        {- Transparent black overlay -}
                                        [ width fill
                                        , height fill
                                        , alpha 0.1
                                        , BG.color <| rgb 0 0 0
                                        , Events.onClick onClose
                                        ]
                                        none
                ]
            <|
                content.element
    }


viewInfoWindow : Props contentMsg -> Shared.Model -> Model -> (Msg -> contentMsg) -> Element contentMsg
viewInfoWindow props shared model toContentMsg =
    let
        topmostPos =
            shared.deviceInfo.window.height - 35

        middlePos =
            {- The +7 is to position the window nicely below the streak number -}
            (shared.deviceInfo.window.height / 2) + 7
    in
    column
        [ width <| px <| (shared.deviceInfo.window.width |> round) - (SafeArea.maxX shared.safeAreaInset * 2)
        , centerX
        , Font.color <| CS.primaryColors.primary
        , BG.color <| rgba255 241 241 230 0.5
        , paddingEach { top = 0, left = 20, right = 20, bottom = 20 }
        , Border.roundEach
            { topLeft = 25
            , topRight = 25
            , bottomLeft = 0
            , bottomRight = 0
            }
        , clip

        --TODO: Prüfen, ob in iOS 18 dann die untere Variante funktioniert.let
        --      Anscheinend funktionierts nicht, wenn beide gleichzeitig gesetzt sind...
        --      Oder eine Weiche einbauen?
        , htmlAttribute <| Html.Attributes.attribute "style" "-webkit-backdrop-filter: blur(16px);"

        -- , htmlAttribute <| Html.Attributes.attribute "style" "backdrop-filter: blur(16px);"
        , height <| px <| round <| shared.deviceInfo.window.height
        , htmlAttribute <| Transition.properties [ Transition.transform 500 [ Transition.easeOutCirc ] ]
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
                            topmostPos - dragDistance

                        Shared.Model.Half ->
                            middlePos - dragDistance

                        Shared.Model.Closed ->
                            0
                    )
                        |> min topmostPos
                        |> max 0

                _ ->
                    0
        , inFront <|
            {- Overlay for receiving touch events; with resize and close buttons in front -}
            let
                safeAreaBottom =
                    SafeArea.paddingEach shared.safeAreaInset
                        |> .bottom
                        |> toFloat

                touchAreaHeight =
                    {- This is needed because of the "swipe from the bottom" - gesture in iOS:
                       If the touch area covers everything including the safeAreaInset.bottom, touch events
                       are triggered when the user does this gesture, but a swipeEnd event isn't happening
                       so the gesture model isn't reset and the InfoWindow stays in the swiped-up position...
                    -}
                    case shared.infoWindowState of
                        Shared.Model.Max ->
                            topmostPos - safeAreaBottom

                        Shared.Model.Half ->
                            middlePos - safeAreaBottom

                        Shared.Model.Closed ->
                            0
            in
            column
                [ width fill
                , height <|
                    px <|
                        round <|
                            touchAreaHeight
                ]
                [ (el
                    [ width fill
                    , height fill
                    , htmlAttribute <| Swipe.onStart SwipeStart
                    , htmlAttribute <| Swipe.onMove Swipe
                    , htmlAttribute <| Swipe.onEnd SwipeEnd
                    ]
                   <|
                    el
                        [ width fill ]
                    <|
                        {- Resize-button -}
                        Input.button
                            [ centerX
                            , height <| px 25
                            , width fill
                            , padding 5
                            ]
                            { onPress = Just OnInfoWindowResize
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
                  )
                    |> E.map toContentMsg
                , el [ paddingXY 15 0, moveUp <| touchAreaHeight - 45 ] <|
                    {- Close button -}
                    Input.button
                        [ Font.size 25
                        , Font.color <| CS.greyOverTransparencyColor shared.colorScheme
                        ]
                        { onPress =
                            case props.overlay of
                                InfoWindow { onClose } ->
                                    Just onClose

                                _ ->
                                    Nothing
                        , label =
                            FeatherIcons.withSize 30 FeatherIcons.x
                                |> FeatherIcons.withStrokeWidth 1.5
                                |> FeatherIcons.toHtml []
                                |> html
                        }
                ]
        ]
        [ el
            {- Header -}
            [ width fill
            , Font.bold
            , Font.center
            , Font.size 20
            , padding 20
            ]
          <|
            case props.overlay of
                InfoWindow { header } ->
                    text header

                _ ->
                    none
        , {- Information content -}
          case props.overlay of
            InfoWindow { info } ->
                info

            _ ->
                none
        , let
            { top, bottom, left, right } =
                SafeArea.paddingEach shared.safeAreaInset
          in
          paragraph [ paddingXY 0 20, Font.size 15 ]
            [ el [ Font.bold ] <| text "Safe Area: "
            , text "top: "
            , text <| String.fromInt top
            , text ", bottom : "
            , text <| String.fromInt bottom
            , text ", left: "
            , text <| String.fromInt left
            , text ", right: "
            , text <| String.fromInt right
            ]

        -- , paragraph [ Font.size 15 ]
        --     [ text "SwipeInitial: "
        --     , text <| String.fromFloat <| Maybe.withDefault 0 <| model.swipeInitialY
        --     , text ", CurrentY: "
        --     , text <| String.fromFloat <| Maybe.withDefault 0 <| model.swipeLocationY
        --     ]
        ]
