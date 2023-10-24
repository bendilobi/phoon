module Layouts.MainNav exposing (Model, Msg, Props, layout)

import Browser.Events
import Components.SimpleAnimatedButton as Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Html.Events as HEvents
import Layout exposing (Layout)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.SafeArea as SafeArea
import Route exposing (Route)
import Route.Path
import Shared
import Task
import Time
import View exposing (View)


type alias Props =
    { header : Maybe String }


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
    , updateButton : Button.Model Msg
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { lastHide = Nothing
      , updateButton = Button.init { onPress = Just CloseUpdate }
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NavButtonClicked Route.Path.Path
    | HiddenAt Time.Posix
    | ShownAt Time.Posix
    | VisibilityChanged Browser.Events.Visibility
    | CloseUpdate
    | ButtonSent Button.Msg


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

        CloseUpdate ->
            ( model, Effect.setUpdating False )

        ButtonSent innerMsg ->
            Button.update
                { msg = innerMsg
                , model = model.updateButton
                , toModel = \button -> { model | updateButton = button }
                }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onVisibilityChange VisibilityChanged



-- VIEW


view : Props -> Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view props shared route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
    , attributes = [ Font.size 17 ]
    , element =
        if shared.appIsUpdating then
            (el [ width fill, height fill ] <|
                el
                    [ centerX
                    , centerY

                    --Todo: stattdessen ordentlich abbrechen
                    , Events.onClick CloseUpdate
                    ]
                <|
                    text "Aktualisiere..."
            )
                |> map toContentMsg

        else if shared.justUpdated then
            (el [ width fill, height fill ] <|
                column [ centerX, centerY, spacing 20 ]
                    --TODO: nur Erfolg melden, wenns wirklich erfolgreich war
                    [ el [ Font.color <| CS.successColor shared.colorScheme, Font.bold ] <|
                        text <|
                            "Update auf Version "
                                ++ shared.currentVersion
                                ++ " erfolgreich!"

                    -- , Components.Button.new { onPress = Just CloseUpdate, label = text "Fertig" }
                    --     |> Components.Button.withLightColor
                    --     |> Components.Button.view shared.colorScheme
                    , Button.new
                        { model = model.updateButton
                        , label = text "Fertig"
                        , toMsg = ButtonSent
                        }
                        |> Button.withLightColor
                        |> Button.view shared.colorScheme
                    ]
            )
                |> map toContentMsg

        else
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
                        viewHeader headerText |> map toContentMsg
                , el
                    [ height fill
                    , width fill
                    , scrollbarY
                    , paddingEach <| SafeArea.paddingX shared.safeAreaInset
                    ]
                    content.element
                , if shared.deviceInfo.orientation == Portrait then
                    viewNavBar shared route |> map toContentMsg

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


viewNavBar : Shared.Model -> Route () -> Element Msg
viewNavBar shared route =
    column
        ([ width fill
         , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
         ]
            ++ CS.navbar shared.colorScheme
        )
        [ row
            [ width fill
            , paddingEach { top = 10, left = 50, right = 50, bottom = 3 }
            ]
            [ el [ alignLeft ] <|
                viewNavButton shared.colorScheme route FeatherIcons.thumbsUp Route.Path.Home_
            , el [ centerX ] <|
                viewNavButton shared.colorScheme route FeatherIcons.play Route.Path.PrepareSession
            , el [ alignRight ] <|
                viewNavButton shared.colorScheme route FeatherIcons.user Route.Path.Information
            ]
        , el
            [ width fill

            -- This is to compensate for the area with rounded screen corners on iPhone XR
            -- TODO: Das portabler machen wie in Tip 5 beschrieben:
            -- https://samselikoff.com/blog/8-tips-to-make-your-website-feel-like-an-ios-app
            , height <| px 41
            ]
            none
        ]


viewNavButton : ColorScheme -> Route () -> FeatherIcons.Icon -> Route.Path.Path -> Element Msg
viewNavButton colorScheme route icon path =
    el
        (if route.path == path then
            [ Font.color <| CS.guideColor colorScheme ]

         else
            []
        )
    <|
        html <|
            FeatherIcons.toHtml [ HEvents.onClick <| NavButtonClicked path ] <|
                FeatherIcons.withSize 30 icon
