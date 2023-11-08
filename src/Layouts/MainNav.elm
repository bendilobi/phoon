module Layouts.MainNav exposing (Model, Msg, Props, layout)

import Browser.Events
import Components.AnimatedButton as Button
import Effect exposing (Effect)
import Element as E exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Layout exposing (Layout)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.SafeArea as SafeArea
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (UpdateState(..))
import Task
import Time
import View exposing (View)


type alias Props =
    { header : Maybe String
    , enableScrolling : Bool
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update
        , view = view props shared route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { lastHide : Maybe Time.Posix
    , updateButton : Button.Model
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { lastHide = Nothing
      , updateButton = Button.init
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NavButtonClicked Route.Path.Path
    | HiddenAt Time.Posix
    | ShownAt Time.Posix
    | VisibilityChanged Browser.Events.Visibility
    | OnCloseUpdateButton Button.Model
    | CancelUpdate


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NavButtonClicked path ->
            ( model, Effect.navigate path )

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
                Effect.navigate Route.Path.Home_

              else
                Effect.none
            )

        OnCloseUpdateButton newState ->
            ( { model | updateButton = newState }
            , if newState == Button.Triggered then
                Effect.setUpdateState <| NotUpdating

              else
                Effect.none
            )

        CancelUpdate ->
            ( model
            , Effect.batch
                [ Effect.setUpdateState <| NotUpdating

                --TODO: Wofür brauche ich reload?
                , Effect.reload
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onVisibilityChange VisibilityChanged



-- VIEW


view : Props -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
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
                        , Events.onClick CancelUpdate
                        ]
                    <|
                        text <|
                            "Aktualisiere..."
                )
                    |> E.map toContentMsg

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
                column
                    (content.attributes
                        ++ [ width fill
                           , height fill
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
         , padding 10
         , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
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
    el [ width fill, height fill ] <|
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
         , paddingEach { top = 4, left = 0, right = 0, bottom = 35 }
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
            [ Font.size 11
            , spacing 4
            , Font.semiBold
            , pointer
            , Events.onClick <| NavButtonClicked path
            ]
            [ el [ centerX ] <|
                html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 29 icon
            , el [ centerX ] <| text label
            ]
