module Layouts.MainNav exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Html.Events as HEvents
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update
        , view = view route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = MotivationClicked
    | SessionClicked
    | InformationClicked


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        MotivationClicked ->
            ( model, Effect.navigate Route.Path.Home_ )

        SessionClicked ->
            ( model, Effect.navigate Route.Path.Session )

        InformationClicked ->
            ( model, Effect.navigate Route.Path.Information )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
    , attributes = []
    , element =
        column
            (content.attributes
                ++ [ width fill
                   , height fill
                   ]
            )
            [ el [ height fill, width fill ] content.element
            , viewNavBar route |> map toContentMsg
            ]
    }


viewNavBar : Route () -> Element Msg
viewNavBar route =
    column
        [ width fill
        , BG.color <| rgb255 247 242 226
        , Font.color <| rgb 0 0 0
        , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
        , Border.color <| rgb255 200 196 166
        ]
        [ row
            [ width fill
            , paddingEach { top = 7, left = 50, right = 50, bottom = 3 }
            ]
            [ el
                [ alignLeft
                , Font.color <|
                    if route.path == Route.Path.Home_ then
                        rgb255 82 155 178

                    else
                        rgb 0 0 0
                ]
              <|
                -- TODO: ggf. den Icon-Code in eine Funktion auslagern... oder eine Komponente...?
                html
                <|
                    FeatherIcons.toHtml [ HEvents.onClick MotivationClicked ] <|
                        FeatherIcons.withSize 30 FeatherIcons.thumbsUp
            , el
                [ centerX
                , Font.color <|
                    if route.path == Route.Path.Session then
                        rgb255 82 155 178

                    else
                        rgb 0 0 0
                ]
              <|
                html <|
                    FeatherIcons.toHtml [ HEvents.onClick SessionClicked ] <|
                        FeatherIcons.withSize 30 FeatherIcons.play
            , el
                [ alignRight
                , Font.color <|
                    if route.path == Route.Path.Information then
                        rgb255 82 155 178

                    else
                        rgb 0 0 0
                ]
              <|
                html <|
                    FeatherIcons.toHtml [ HEvents.onClick InformationClicked ] <|
                        FeatherIcons.withSize 30 FeatherIcons.info
            ]
        , el
            [ width fill

            -- This is to compensate for the area with rounded screen corners on iPhone XR
            , height <| px 41
            ]
            none
        ]
