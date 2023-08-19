module Pages.Phases.Retention exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Layouts
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Lib.Tools as Tools
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.SessionControls
        {}



-- INIT


type alias Model =
    {}


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( {}
    , Effect.batch
        [ Effect.playSound
        , Effect.resultsUpdated <| SessionResults.addRetention shared.results
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            ( model, Effect.resultsUpdated <| SessionResults.incrementCurrentRetention shared.results )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
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
                [ el [] <| text <| Tools.formatSeconds <| SessionResults.currentRetentionTime shared.results
                ]
            ]
    }
