module Pages.Phases.Retention exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Layouts
import Lib.Tools as Tools
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Time
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
    { seconds : Int }


init : () -> ( Model, Effect Msg )
init () =
    ( { seconds = 0 }
    , Effect.playSound
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | seconds = model.seconds + 1 }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> View Msg
view model =
    { title = "Zoff - Session"
    , attributes =
        [ Background.color <| rgb255 38 86 86
        , Font.color <| rgb255 255 255 255
        ]
    , element =
        column
            [ width fill
            , height fill
            ]
            [ column [ centerX, centerY ]
                [ el [] <| text <| Tools.formatSeconds model.seconds
                ]
            ]
    }
