module Layouts.MainNav exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Html.Events as HEvents
import Layout exposing (Layout)
import Lib.ColorScheme as CS exposing (ColorScheme)
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
        , view = view shared route
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


view : Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view shared route { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
    , attributes = [ Font.size 17 ]
    , element =
        column
            (content.attributes
                ++ [ width fill
                   , height fill
                   ]
            )
            [ el
                [ height fill
                , width fill
                , scrollbarY
                ]
                content.element
            , viewNavBar shared route |> map toContentMsg
            ]
    }


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
                -- viewNavButton shared.colorScheme route FeatherIcons.info Route.Path.Information
                viewNavButton shared.colorScheme route FeatherIcons.settings Route.Path.Information
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
