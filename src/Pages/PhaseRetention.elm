module Pages.PhaseRetention exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.SessionControls
        {}



-- INIT


type alias Model =
    { paused : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { paused = False
      }
    , Effect.playSound
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Zoff - Session"
    , attributes = []
    , element =
        column
            [ width fill
            , height fill
            , Background.color <| rgb255 38 86 86
            , Font.color <| rgb255 255 255 255
            ]
            [ column [ centerX, centerY ]
                [ el [] <| text "Retention..."
                , if model.paused then
                    el [] <| text "Pausiert"

                  else
                    none
                ]
            ]
    }
