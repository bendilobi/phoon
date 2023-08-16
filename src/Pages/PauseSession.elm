module Pages.PauseSession exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
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
        , update = update
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
    = EndSession


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EndSession ->
            ( model
              -- , Effect.none
              -- TODO: wenn das hier ist, wird das beim Swipe ausgelöst, auch wenn nirgendwo eine EndSession-Message gesendet wird...
            , Effect.pushRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing }
            )



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
            , Background.color <| rgb255 172 115 122
            , Font.color <| rgb255 255 255 255
            ]
            [ column [ centerX, centerY ]
                [ el [] <| text "Pause"

                -- [ button []
                --     { onPress = Just EndSession
                --     , label = text "Übung beenden"
                --     }
                ]
            ]
    }
