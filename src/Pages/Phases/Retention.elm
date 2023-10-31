module Pages.Phases.Retention exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.Session as Session
import Lib.SessionResults as SessionResults
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
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.SessionControls
        { showCurrentCycle = Just <| SessionResults.finishedCycles shared.results + 1
        }



-- INIT


type alias Model =
    {}


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( {}
    , Effect.playSound Session.RetentionSound
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
    { title = "Retentions-Phase"
    , attributes =
        CS.phaseRetention shared.colorScheme
    , element =
        el [ width fill, height fill ] <|
            el [ Font.size 30, centerX, centerY ] <|
                text <|
                    Millis.toString <|
                        Millis.fromSeconds <|
                            SessionResults.currentRetentionTime shared.results
    }
