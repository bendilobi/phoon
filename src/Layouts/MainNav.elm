module Layouts.MainNav exposing (Model, Msg, Overlay(..), Props, layout, map)

import Browser.Events
import Components.AnimatedButton as Button
import Delay
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
import Lib.PageFading as Fading exposing (FadeState, Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Swipe as Swipe
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (UpdateState(..))
import Simple.Transition as Transition
import Task
import Time
import View exposing (View)


type alias Props contentMsg =
    { header : Maybe String
    , enableScrolling : Bool
    , fadeOut : Fading.Trigger
    , overlay : Overlay contentMsg
    }


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
        { init = init shared
        , update = update shared
        , view = view props shared route
        , subscriptions = subscriptions
        }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { header = props.header
    , enableScrolling = props.enableScrolling
    , fadeOut = props.fadeOut
    , overlay =
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
    { lastHide : Maybe Time.Posix
    , updateButton : Button.Model
    , updateAcknowledged : Bool
    , fadeState : FadeState
    , swipeGesture : Swipe.Gesture
    , swipeInitialY : Maybe Float
    , swipeLocationY : Maybe Float
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    ( { lastHide = Nothing
      , updateButton = Button.init
      , updateAcknowledged = False
      , fadeState = Fading.init shared.fadeIn
      , swipeGesture = Swipe.blanco
      , swipeInitialY = Nothing
      , swipeLocationY = Nothing
      }
    , Effect.sendCmd <| Fading.initCmd shared.fadeIn ToggleFadeIn
    )


updateFadeOutDuration =
    1000



-- UPDATE


type Msg
    = NavButtonClicked Route.Path.Path
    | HiddenAt Time.Posix
    | ShownAt Time.Posix
    | VisibilityChanged Browser.Events.Visibility
    | OnCloseUpdateButton Button.Model
    | UpdateFinished
    | ToggleFadeIn Fading.Trigger
    | OnInfoWindowResize
    | SwipeStart Swipe.Event
    | Swipe Swipe.Event
    | SwipeEnd Swipe.Event


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        NavButtonClicked path ->
            ( model, Effect.navigate NoFade path )

        VisibilityChanged visibility ->
            ( model
            , case visibility of
                Browser.Events.Hidden ->
                    Effect.sendCmd <| Task.perform HiddenAt Time.now

                Browser.Events.Visible ->
                    Effect.sendCmd <| Task.perform ShownAt Time.now
            )

        HiddenAt time ->
            ( { model | lastHide = Just time }
            , Effect.none
            )

        ShownAt time ->
            let
                lastHide =
                    model.lastHide
                        |> Maybe.map Time.posixToMillis
                        |> Maybe.withDefault 0
            in
            ( model
            , if Time.posixToMillis time - lastHide > 900000 then
                Effect.navigate NoFade Route.Path.Home_

              else
                Effect.none
            )

        OnCloseUpdateButton newState ->
            ( { model
                | updateButton = newState
                , updateAcknowledged = newState == Button.Triggered
              }
            , if newState == Button.Triggered then
                --- Wait until the animation finished. Cannot use the "transitionend"
                --- event since it is triggered by the button within the animated
                --- container...
                Effect.sendCmd <| Delay.after updateFadeOutDuration UpdateFinished

              else
                Effect.none
            )

        UpdateFinished ->
            ( { model | updateAcknowledged = False }
            , Effect.setUpdateState <| NotUpdating
            )

        ToggleFadeIn fade ->
            ( { model | fadeState = Fading.update fade }
            , Effect.sendCmd <| Fading.updateCmd fade ToggleFadeIn
            )

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
    Browser.Events.onVisibilityChange VisibilityChanged



-- VIEW


view : Props contentMsg -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
    , attributes = [ Font.size 17 ]
    , element =
        case shared.updateState of
            Updating _ ->
                (el [ width fill, height fill ] <|
                    el
                        [ centerX
                        , centerY
                        , Events.onClick UpdateFinished
                        ]
                    <|
                        text <|
                            "Aktualisiere..."
                )
                    |> E.map toContentMsg

            _ ->
                column
                    (content.attributes
                        ++ [ width fill
                           , height fill
                           , clip
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
                                        case shared.updateState of
                                            JustUpdated ->
                                                viewUpdateResult shared
                                                    model
                                                    { color = CS.successColor shared.colorScheme
                                                    , message = "Update auf Version " ++ Shared.appVersion ++ " erfolgreich!"
                                                    , label = "Fertig"
                                                    }
                                                    |> E.map toContentMsg

                                            UpdateFailed errorMessage ->
                                                viewUpdateResult shared
                                                    model
                                                    { color = CS.actionNeededColor shared.colorScheme
                                                    , message = errorMessage
                                                    , label = "Später versuchen"
                                                    }
                                                    |> E.map toContentMsg

                                            _ ->
                                                Fading.fadeOverlay props.fadeOut model.fadeState

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
                           , below <| viewInfoWindow props shared model toContentMsg
                           ]
                    )
                    [ case props.header of
                        Nothing ->
                            none

                        Just headerText ->
                            viewHeader headerText |> E.map toContentMsg
                    , el
                        ([ height fill
                         , width fill
                         , paddingEach <| SafeArea.paddingX shared.safeAreaInset
                         ]
                            ++ (if props.enableScrolling then
                                    --- Continuous scrolling by flicking on touch devices
                                    --- seems to produce scrolling events even during page
                                    --- change, so the new page continues the unfinished
                                    --- scrolling process of the previous page
                                    --- This leads to broken appearance of the new page
                                    --- if it is scrollable. So we enable scrollbars only
                                    --- on pages that need them.
                                    [ scrollbarY ]

                                else
                                    []
                               )
                        )
                        content.element
                    , if shared.deviceInfo.orientation == Portrait then
                        viewNavBar shared route |> E.map toContentMsg

                      else
                        none
                    ]
    }


viewHeader : String -> Element msg
viewHeader headerText =
    el
        ([ width fill
         , Font.center
         , Font.bold
         , paddingEach { bottom = 10, top = 0, left = 0, right = 0 }
         ]
            ++ CS.primary
        )
    <|
        text headerText


viewUpdateResult :
    Shared.Model
    -> Model
    -> { color : Color, message : String, label : String }
    -> Element Msg
viewUpdateResult shared model { color, message, label } =
    el
        [ width fill
        , height fill
        , if model.updateAcknowledged then
            alpha 0

          else
            alpha 1
        , BG.color <| rgb 1 1 1
        , htmlAttribute <|
            Transition.properties
                [ Transition.opacity updateFadeOutDuration [ Transition.easeInQuint ] -- Transition.easeInQuart ]
                ]

        --- This doesn't work because it reacts on the transition of the button...
        -- , htmlAttribute <| HEvents.on "transitionend" <| Decode.succeed UpdateFinished
        ]
    <|
        column [ centerX, centerY, spacing 20 ]
            [ el [ Font.color color, Font.bold ] <|
                text message
            , Button.new
                { model = model.updateButton
                , label = text label
                , onPress = OnCloseUpdateButton
                }
                |> Button.withLightColor
                |> Button.view shared.colorScheme
            ]


viewNavBar : Shared.Model -> Route () -> Element Msg
viewNavBar shared route =
    row
        ([ width fill
         , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
         , paddingEach { top = 5, left = 0, right = 0, bottom = 35 }
         , spaceEvenly
         ]
            ++ CS.navbar shared.colorScheme
        )
    <|
        let
            viewButton =
                viewNavButton shared.colorScheme route
        in
        --- Elements with zero width to make elm-ui space them correctly...
        [ el [ width <| px 0 ] none
        , viewButton "Motivieren" FeatherIcons.thumbsUp Route.Path.Home_
        , el [ width <| px 0 ] none
        , viewButton "Praktizieren" FeatherIcons.play Route.Path.PrepareSession
        , el [ width <| px 0 ] none
        , viewButton "Optimieren" FeatherIcons.user Route.Path.Information
        , el [ width <| px 0 ] none
        ]


viewNavButton : ColorScheme -> Route () -> String -> FeatherIcons.Icon -> Route.Path.Path -> Element Msg
viewNavButton colorScheme route label icon path =
    el
        (if route.path == path then
            [ Font.color <| CS.guideColor colorScheme ]

         else
            []
        )
    <|
        column
            [ Font.size 10
            , spacing 5
            , Font.semiBold
            , pointer
            , Events.onClick <| NavButtonClicked path
            ]
            [ el [ centerX ] <|
                html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 27 icon
            , el [ centerX ] <| text label
            ]


viewInfoWindow : Props contentMsg -> Shared.Model -> Model -> (Msg -> contentMsg) -> Element contentMsg
viewInfoWindow props shared model toContentMsg =
    column
        [ width fill
        , BG.color <| rgba 1 1 1 0.5
        , Font.color <| rgb 0 0 0
        , paddingEach { top = 0, left = 20, right = 20, bottom = 20 }

        -- , spacing 10
        , Border.roundEach
            { topLeft = 25
            , topRight = 25
            , bottomLeft = 0
            , bottomRight = 0
            }

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

                        topmostPos =
                            shared.deviceInfo.window.height - 35

                        middlePos =
                            shared.deviceInfo.window.height / 2
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
            column [ width fill, height fill ]
                [ (el
                    [ width fill
                    , height fill
                    , htmlAttribute <| Swipe.onStart SwipeStart
                    , htmlAttribute <| Swipe.onMove Swipe
                    , htmlAttribute <| Swipe.onEnd SwipeEnd

                    -- , BG.color <| rgb 1 0 0
                    ]
                   <|
                    el
                        [ width fill ]
                    <|
                        {- Resize-button -}
                        Input.button
                            [ centerX
                            , height <| px 25

                            -- , width <| px 200
                            , width fill

                            -- , BG.color <| rgb 1 0 0
                            , padding 5
                            ]
                            { onPress = Just OnInfoWindowResize
                            , label =
                                el
                                    [ height <| px 5
                                    , width <| px 40
                                    , BG.color <| rgb 0.4 0.4 0.4
                                    , Border.rounded 4
                                    , alignTop
                                    ]
                                    none
                            }
                  )
                    |> E.map toContentMsg
                , el [ paddingXY 15 0, moveUp <| shared.deviceInfo.window.height - 45 ] <|
                    Input.button
                        [ Font.size 25
                        ]
                        { onPress =
                            case props.overlay of
                                InfoWindow { onClose } ->
                                    Just onClose

                                _ ->
                                    Nothing
                        , label =
                            html <|
                                FeatherIcons.toHtml [] <|
                                    FeatherIcons.withSize 30
                                        FeatherIcons.x
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
        ]
