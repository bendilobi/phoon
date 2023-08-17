module Pages.Home_ exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Input exposing (button)
import Html
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = SessionStartPressed


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        SessionStartPressed ->
            ( model
              -- , Effect.pushRoute
              --     { path = Route.Path.Breathsession
              --     , query = Dict.empty
              --     , hash = Nothing
              --     }
              -- , Effect.playSound
            , Effect.batch
                [ Effect.playSound
                , Effect.replaceRoute
                    { path = Route.Path.Breathsession
                    , query = Dict.empty
                    , hash = Nothing
                    }
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Zoff - WHM the Zen Way"
    , attributes = []
    , element =
        column
            [ width fill
            , height fill
            , Background.color <| rgb255 200 196 183
            ]
            [ button
                [ centerX, centerY ]
                { onPress = Just SessionStartPressed
                , label = text "Los geht's!"
                }
            ]

    -- [ link
    --     [ centerX
    --     , centerY
    --     ]
    --     { url = "/breathsession"
    --     , label = text "Hallöchen bei Zoff!"
    --     }
    -- ]
    }
