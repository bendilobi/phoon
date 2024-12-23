module Layouts.BaseLayout.MainNav exposing (Model, Msg, Props, SubPage, layout, map, viewHeaderButton)

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
import Lib.Texts as Texts
import Lib.Utils as Utils exposing (MainTask(..))
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (UpdateState(..))
import Simple.Transition as Transition
import String exposing (right)
import String.Format
import Task
import Time
import Version
import View exposing (View)


type alias Props contentMsg =
    { header : Maybe String
    , enableScrolling : Bool
    , fadeOut : Fading.Trigger
    , subPage : Maybe (SubPage contentMsg)
    , headerIcon : Maybe (Element contentMsg)
    , overlay : Layouts.BaseLayout.Overlay contentMsg
    }


type alias SubPage contentMsg =
    { header : String
    , content : Element contentMsg
    }


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
    , headerIcon = Maybe.map (\i -> E.map fn i) props.headerIcon
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
    | TimedSwipeStart Swipe.Event Time.Posix
    | SwipeMove Swipe.Event
    | TimedSwipeMove Swipe.Event Time.Posix
    | SwipeEnd Swipe.Event
    | TimedSwipeEnd Swipe.Event Time.Posix
    | OnSubPageBack


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
            ( model
            , Effect.sendCmd <| Task.perform (TimedSwipeStart event) Time.now
            )

        TimedSwipeStart event time ->
            ( { model
                | swipeGesture = Swipe.recordWithTime event time model.swipeGesture
                , swipeInitialX = Just <| .x <| Swipe.locate event
              }
            , Effect.none
            )

        SwipeMove event ->
            ( model
            , Effect.sendCmd <| Task.perform (TimedSwipeMove event) Time.now
            )

        TimedSwipeMove event time ->
            ( { model
                | swipeGesture = Swipe.recordWithTime event time model.swipeGesture
                , swipeLocationX = Swipe.locate event |> .x |> Just
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
                    shared.deviceInfo.window.width - (shared.deviceInfo.window.width / 4)

                closeSubPage =
                    Swipe.isRightSwipe swipeThreshold gesture || Swipe.isRightFlick gesture
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
            ( model, Effect.toggleSubPage )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onVisibilityChange VisibilityChanged



-- VIEW


view : Props contentMsg -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title
    , attributes = [ Font.size 15 ]
    , element =
        case shared.updateState of
            Updating _ ->
                (el [ width fill, height fill, BG.color <| rgb 1 1 1 ] <|
                    el
                        [ centerX
                        , centerY
                        , Events.onClick UpdateFinished
                        ]
                    <|
                        text <|
                            Texts.updating shared.appLanguage
                )
                    |> E.map toContentMsg

            _ ->
                column
                    [ width fill
                    , height fill
                    , Font.size 17
                    , inFront <|
                        case shared.updateState of
                            JustUpdated ->
                                viewUpdateResult shared
                                    model
                                    { color = CS.successColor shared.colorScheme
                                    , message =
                                        Texts.updateSuccessfull shared.appLanguage
                                            |> String.Format.value Version.appVersion
                                    , label = Texts.done shared.appLanguage
                                    }
                                    |> E.map toContentMsg

                            UpdateFailed errorMessage ->
                                viewUpdateResult shared
                                    model
                                    { color = CS.actionNeededColor shared.colorScheme
                                    , message = errorMessage
                                    , label = Texts.tryLater shared.appLanguage
                                    }
                                    |> E.map toContentMsg

                            _ ->
                                Fading.fadeOverlay props.fadeOut model.fadeState
                    ]
                    [ el
                        [ width fill
                        , height fill
                        , inFront <| viewSubpage props shared model props.subPage toContentMsg
                        ]
                      <|
                        column
                            [ width fill
                            , height fill
                            , inFront <|
                                {- Transparent overlay while subPage is shown -}
                                let
                                    dragDistance =
                                        case ( model.swipeInitialX, model.swipeLocationX ) of
                                            ( Just initialPos, Just currentX ) ->
                                                currentX - initialPos

                                            ( _, _ ) ->
                                                0
                                in
                                el
                                    [ width fill
                                    , height <|
                                        if shared.subPageShown then
                                            fill

                                        else
                                            px 0
                                    , BG.color <| rgb 0 0 0
                                    , alpha <|
                                        if shared.subPageShown && not shared.subPageClosingInProgress then
                                            0.3 - dragDistance * 0.3 / shared.deviceInfo.window.width

                                        else
                                            0
                                    , htmlAttribute <|
                                        case model.swipeInitialX of
                                            Nothing ->
                                                Transition.properties [ Transition.opacity Shared.subPageClosingTime [ Transition.easeOutExpo ] ]

                                            _ ->
                                                Html.Attributes.hidden False
                                    ]
                                    none
                            ]
                            [ case props.header of
                                Nothing ->
                                    none

                                Just headerText ->
                                    viewHeader shared headerText props.headerIcon
                            , el
                                ([ width fill
                                 , height fill
                                 , Border.roundEach <|
                                    let
                                        rad =
                                            case shared.infoWindowState of
                                                Shared.Model.Max ->
                                                    15

                                                _ ->
                                                    0
                                    in
                                    { topLeft = rad, topRight = rad, bottomLeft = 0, bottomRight = 0 }
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
                                            [ scrollbarY

                                            {- This fixes a problem with recent versions of Chromium/Chrome
                                               (see https://github.com/mdgriffith/elm-ui/issues/367)
                                            -}
                                            , htmlAttribute <| Html.Attributes.style "min-height" "auto"
                                            ]

                                        else
                                            []
                                       )
                                    ++ content.attributes
                                )
                                content.element
                            ]
                    , if shared.deviceInfo.orientation == Landscape && shared.pointerIsMouse == Just False then
                        --TODO: Maybe it would be better to determine whether to show the navbar based on screen
                        --      size...
                        none

                      else
                        viewNavBar shared route |> E.map toContentMsg
                    ]
    }


viewHeader : Shared.Model -> String -> Maybe (Element contentMsg) -> Element contentMsg
viewHeader shared headerText icon =
    let
        { right } =
            SafeArea.paddingEach shared.safeAreaInset
    in
    el
        ([ width fill
         , Font.center
         , Font.bold
         , Font.size 17
         , height <| px 40
         , paddingEach { bottom = 10, top = 4, left = 0, right = 0 }
         , inFront <|
            el [ width fill, centerY ] <|
                el
                    [ alignRight
                    , paddingEach { bottom = 5, top = 0, left = 0, right = 10 + right }
                    ]
                <|
                    Maybe.withDefault none icon
         ]
            ++ CS.primary
        )
    <|
        el [ width fill, centerX, centerY ] <|
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
        , BG.color <| CS.primaryColors.primary
        , htmlAttribute <|
            Transition.properties
                [ Transition.opacity updateFadeOutDuration [ Transition.easeInQuint ] -- Transition.easeInQuart ]
                ]

        --- This doesn't work because it reacts on the transition of the button...
        -- , htmlAttribute <| HEvents.on "transitionend" <| Decode.succeed UpdateFinished
        ]
    <|
        column
            [ centerX
            , centerY
            , spacing 20
            , paddingXY 30 0
            ]
            [ paragraph [ Font.color color, Font.center, Font.bold ]
                [ text message
                ]
            , Button.new
                { model = model.updateButton
                , label = text label
                , onPress = OnCloseUpdateButton
                }
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
        , viewButton (Texts.motivate shared.appLanguage) Motivate Route.Path.Home_
        , el [ width <| px 0 ] none
        , viewButton (Texts.practice shared.appLanguage) Practice Route.Path.Practice
        , el [ width <| px 0 ] none
        , viewButton (Texts.optimize shared.appLanguage) Optimize Route.Path.Optimize
        , el [ width <| px 0 ] none
        ]


viewNavButton : ColorScheme -> Route () -> String -> MainTask -> Route.Path.Path -> Element Msg
viewNavButton colorScheme route label mainTask path =
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
                            Utils.mainTaskIcon (route.path == path) mainTask
            , el [ centerX ] <| text label
            ]


viewSubpage : Props contentMsg -> Shared.Model -> Model -> Maybe (SubPage contentMsg) -> (Msg -> contentMsg) -> Element contentMsg
viewSubpage props shared model subPage toContentMsg =
    let
        { left, right } =
            SafeArea.paddingEach shared.safeAreaInset
    in
    column
        [ width <| px <| round shared.deviceInfo.window.width
        , height fill

        {- The natural way to render the subPage would be to use "onRight" and then "moveLeft" to show it. Unfortunately,
           elements that are "onRight" are shown above all other elements, i.e. also above our modal dialogs that are managed
           by the BaseLayout. So we have to use "inFront" and move the subPage to the right by default. This again triggers
           the Transition animation which leads to the subPage sliding to the right if the MainNav is initially rendered.
           Our workaround is to make the subPage invisible as long as there is no content anyway:
        -}
        , transparent <| props.subPage == Nothing
        , moveRight <|
            let
                dragDistance =
                    case ( model.swipeInitialX, model.swipeLocationX ) of
                        ( Just initialPos, Just currentX ) ->
                            currentX - initialPos

                        ( _, _ ) ->
                            0
            in
            if shared.subPageShown && not shared.subPageClosingInProgress then
                0 + max dragDistance 0

            else
                shared.deviceInfo.window.width
        , htmlAttribute <|
            case model.swipeLocationX of
                {- Suppress animation while swiping -}
                Nothing ->
                    Transition.properties [ Transition.transform Shared.subPageClosingTime [ Transition.easeOutExpo ] ]

                _ ->
                    Html.Attributes.hidden False
        , inFront <|
            E.map toContentMsg <|
                el
                    [ height fill
                    , width <| px 30
                    , htmlAttribute <| Swipe.onStart SwipeStart
                    , htmlAttribute <| Swipe.onMove SwipeMove
                    , htmlAttribute <| Swipe.onEnd SwipeEnd
                    ]
                    none

        {- Prevent the page content behind the subPage from being visible when overscrolling: -}
        , behindContent <| el ([ width fill, height fill ] ++ CS.primaryInformation shared.colorScheme) none
        ]
        [ el
            ([ width fill
             , height <| px 40
             , paddingXY 20 9
             , inFront <|
                E.map toContentMsg <|
                    Input.button [ alignLeft, paddingXY left 0 ]
                        { label =
                            row []
                                [ FeatherIcons.chevronLeft
                                    |> FeatherIcons.withSize 33
                                    |> FeatherIcons.toHtml []
                                    |> html
                                , text <| Texts.back shared.appLanguage
                                ]
                        , onPress = Just OnSubPageBack
                        }
             ]
                ++ CS.primary
            )
          <|
            el [ centerX, Font.semiBold ] <|
                text <|
                    Maybe.withDefault "" <|
                        Maybe.map .header subPage
        , el
            [ width fill
            , height fill
            , scrollbarY
            , htmlAttribute <| Html.Attributes.style "min-height" "auto"
            , paddingEach { left = left, right = right, bottom = 0, top = 0 }
            ]
          <|
            Maybe.withDefault none <|
                Maybe.map .content subPage
        ]


{-| To be called from within pages using the Layout for passing the information to the
layout props
-}
viewHeaderButton : FeatherIcons.Icon -> contentMsg -> Element contentMsg
viewHeaderButton icon onTap =
    Input.button [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ]
        { label =
            icon
                |> FeatherIcons.withSize 25
                |> FeatherIcons.withStrokeWidth 1.5
                |> FeatherIcons.toHtml []
                |> html
        , onPress = Just onTap
        }
