module Layouts.BaseLayout.MainNav exposing (Model, Msg, Props, SubPage, layout, map)

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
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.PageFading as Fading exposing (FadeState, Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Swipe as Swipe
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (UpdateState(..))
import Simple.Transition as Transition
import Svg
import Svg.Attributes
import Task
import Time
import View exposing (View)


type alias Props contentMsg =
    { header : Maybe String
    , enableScrolling : Bool
    , fadeOut : Fading.Trigger
    , subPage : Maybe (SubPage contentMsg)
    , overlay : Layouts.BaseLayout.Overlay contentMsg
    }


type alias SubPage contentMsg =
    { header : String
    , content : Element contentMsg
    }


subPageClosingTime =
    500


layout : Props contentMsg -> Shared.Model -> Route () -> Layout (Layouts.BaseLayout.Props contentMsg) Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init shared
        , update = update shared
        , view = view props shared route
        , subscriptions = subscriptions
        }
        |> Layout.withParentProps { overlay = props.overlay }


map : (msg1 -> msg2) -> Props msg1 -> Props msg2
map fn props =
    { header = props.header
    , enableScrolling = props.enableScrolling
    , fadeOut = props.fadeOut
    , subPage =
        Maybe.map
            (\{ header, content } ->
                { header = header
                , content = E.map fn content
                }
            )
            props.subPage
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
    { lastHide : Maybe Time.Posix
    , updateButton : Button.Model
    , updateAcknowledged : Bool
    , fadeState : FadeState
    , swipeGesture : Swipe.Gesture
    , swipeInitialX : Maybe Float
    , swipeLocationX : Maybe Float
    , subPageClosingInProgress : Bool
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    ( { lastHide = Nothing
      , updateButton = Button.init
      , updateAcknowledged = False
      , fadeState = Fading.init shared.fadeIn
      , swipeGesture = Swipe.blanco
      , swipeInitialX = Nothing
      , swipeLocationX = Nothing
      , subPageClosingInProgress = False
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
    | SwipeStart Swipe.Event
    | Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | OnSubPageBack
    | CloseSubPage


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
            ( { model
                | lastHide = Just time
              }
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

        SwipeStart event ->
            ( { model
                | swipeGesture = Swipe.record event model.swipeGesture
                , swipeInitialX = Just <| .x <| Swipe.locate event
              }
            , Effect.none
            )

        Swipe event ->
            ( { model
                | swipeGesture = Swipe.record event model.swipeGesture
                , swipeLocationX = Swipe.locate event |> .x |> Just
              }
            , Effect.none
            )

        SwipeEnd event ->
            let
                gesture =
                    Swipe.record event model.swipeGesture

                swipeThreshold =
                    shared.deviceInfo.window.width - (shared.deviceInfo.window.width / 4)

                closeSubPage =
                    Swipe.isRightSwipe swipeThreshold gesture
            in
            ( { model
                | swipeGesture = Swipe.blanco
                , swipeInitialX = Nothing
                , swipeLocationX = Nothing
              }
            , if closeSubPage then
                Effect.toggleSubPage

              else
                Effect.none
            )

        OnSubPageBack ->
            ( { model | subPageClosingInProgress = True }
            , Effect.sendCmd <| Delay.after subPageClosingTime CloseSubPage
            )

        CloseSubPage ->
            ( { model | subPageClosingInProgress = False }
            , Effect.toggleSubPage
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onVisibilityChange VisibilityChanged



-- VIEW


view : Props contentMsg -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title
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
                           , inFront <|
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
                           ]
                    )
                    [ column
                        [ width fill
                        , height fill
                        , inFront <| viewSubpage shared model props.subPage toContentMsg
                        ]
                        [ case props.header of
                            Nothing ->
                                none

                            Just headerText ->
                                viewHeader headerText |> E.map toContentMsg
                        , el
                            ([ width fill
                             , height fill
                             , paddingEach <|
                                if shared.deviceInfo.orientation == Portrait then
                                    { top = 0, bottom = 0, left = 0, right = 0 }

                                else
                                    SafeArea.paddingEach shared.safeAreaInset
                             ]
                                ++ (if props.enableScrolling then
                                        --- Continuous scrolling by flicking on touch devices
                                        --- seems to produce scrolling events even during page
                                        --- change, so the new page continues the unfinished
                                        --- scrolling process of the previous page
                                        --- This leads to broken appearance of the new page
                                        --- if it is scrollable. So we enable scrollbars only
                                        --- on pages that need them.
                                        --TODO: Das funktioniert in Chromium nicht mehr. Ist das ein Bug dort?
                                        --      Selbst wenn ich height festsetze und clip dazu, ist der content-
                                        --      Bereicht so groß wie der Inhalt und die gesamte App scrollt...
                                        [ scrollbarY ]

                                    else
                                        []
                                   )
                            )
                            content.element
                        ]
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
    let
        playFilled =
            [ Svg.polygon [ Svg.Attributes.fill "currentColor", Svg.Attributes.points "5 3 19 12 5 21 5 3" ] []
            ]
                |> FeatherIcons.customIcon

        thumbsUpFilled =
            [ Svg.path [ Svg.Attributes.fill "currentColor", Svg.Attributes.d "M14 9V5a3 3 0 0 0-3-3l-4 9v11h11.28a2 2 0 0 0 2-1.7l1.38-9a2 2 0 0 0-2-2.3zM7 22H4a2 2 0 0 1-2-2v-7a2 2 0 0 1 2-2h3" ] []
            ]
                |> FeatherIcons.customIcon

        userFilled =
            [ Svg.path [ Svg.Attributes.fill "currentColor", Svg.Attributes.d "M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2" ] []
            , Svg.circle [ Svg.Attributes.fill "currentColor", Svg.Attributes.cx "12", Svg.Attributes.cy "7", Svg.Attributes.r "4" ] []
            ]
                |> FeatherIcons.customIcon
    in
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
        , viewButton "Motivieren" FeatherIcons.thumbsUp thumbsUpFilled Route.Path.Home_
        , el [ width <| px 0 ] none
        , viewButton "Praktizieren" FeatherIcons.play playFilled Route.Path.PrepareSession
        , el [ width <| px 0 ] none
        , viewButton "Optimieren" FeatherIcons.user userFilled Route.Path.Information
        , el [ width <| px 0 ] none
        ]


viewNavButton : ColorScheme -> Route () -> String -> FeatherIcons.Icon -> FeatherIcons.Icon -> Route.Path.Path -> Element Msg
viewNavButton colorScheme route label icon iconFilled path =
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
                        FeatherIcons.withSize 27 <|
                            if route.path == path then
                                iconFilled

                            else
                                icon
            , el [ centerX ] <| text label
            ]


viewSubpage : Shared.Model -> Model -> Maybe (SubPage contentMsg) -> (Msg -> contentMsg) -> Element contentMsg
viewSubpage shared model subPage toContentMsg =
    column
        [ width fill
        , height fill
        , moveRight <|
            let
                dragDistance =
                    case ( model.swipeInitialX, model.swipeLocationX ) of
                        ( Just initialPos, Just currentX ) ->
                            currentX - initialPos

                        ( _, _ ) ->
                            0
            in
            if shared.subPageShown && not model.subPageClosingInProgress then
                dragDistance

            else
                shared.deviceInfo.window.width
        , htmlAttribute <|
            case model.swipeInitialX of
                {- Suppress animation while swiping -}
                Nothing ->
                    Transition.properties [ Transition.transform subPageClosingTime [ Transition.easeOutExpo ] ]

                _ ->
                    Html.Attributes.hidden False
        , inFront <|
            E.map toContentMsg <|
                el
                    [ height fill
                    , width <| px 30
                    , htmlAttribute <| Swipe.onStart SwipeStart
                    , htmlAttribute <| Swipe.onMove Swipe
                    , htmlAttribute <| Swipe.onEnd SwipeEnd
                    ]
                    none
        ]
        [ el
            ([ width fill
             , height <| px 50
             , paddingXY 20 0
             , inFront <|
                E.map toContentMsg <|
                    Input.button [ alignLeft, centerY ]
                        { label =
                            row []
                                [ FeatherIcons.chevronLeft
                                    |> FeatherIcons.withSize 30
                                    |> FeatherIcons.toHtml []
                                    |> html
                                , text "Zurück"
                                ]
                        , onPress = Just OnSubPageBack
                        }
             ]
                ++ CS.primary
            )
          <|
            el [ centerX, centerY, Font.bold ] <|
                text <|
                    Maybe.withDefault "" <|
                        Maybe.map .header subPage
        , el
            [ width fill
            , height fill
            , scrollbarY
            ]
          <|
            Maybe.withDefault none <|
                Maybe.map .content subPage
        ]
