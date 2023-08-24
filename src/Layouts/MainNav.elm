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
    = NavButtonClicked Route.Path.Path


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NavButtonClicked path ->
            ( model, Effect.navigate path )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
    , attributes = [ Font.size 17 ]
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
            , paddingEach { top = 10, left = 50, right = 50, bottom = 3 }
            ]
            [ el [ alignLeft ] <|
                viewNavButton route FeatherIcons.thumbsUp Route.Path.Home_
            , el [ centerX ] <|
                viewNavButton route FeatherIcons.play Route.Path.PrepareSession
            , el [ alignRight ] <|
                viewNavButton route FeatherIcons.info Route.Path.Information
            ]
        , el
            [ width fill

            -- This is to compensate for the area with rounded screen corners on iPhone XR
            , height <| px 41
            ]
            none
        ]


viewNavButton : Route () -> FeatherIcons.Icon -> Route.Path.Path -> Element Msg
viewNavButton route icon path =
    el
        [ Font.color <|
            if route.path == path then
                rgb255 82 155 178

            else
                rgb 0 0 0
        ]
    <|
        html <|
            FeatherIcons.toHtml [ HEvents.onClick <| NavButtonClicked path ] <|
                FeatherIcons.withSize 30 icon
