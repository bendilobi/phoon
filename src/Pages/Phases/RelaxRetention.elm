module Pages.Phases.RelaxRetention exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.BreathingSession as BS
import Lib.Tools as Tools
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
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
    ( { seconds = 1 }
    , Effect.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            ( { model | seconds = model.seconds + 1 }
            , if model.seconds == 15 then
                Tools.navigateNext shared.session

              else
                Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> View Msg
view model =
    { title = "Relax Retention Phase"
    , attributes =
        [ BG.color <| rgb255 46 69 131
        , Font.color <| rgb255 255 255 255
        ]
    , element =
        column
            [ width fill
            , height fill
            ]
            [ column [ centerX, centerY ]
                [ el
                    [ paddingXY 0 10
                    , Font.size 40
                    , Font.bold
                    , centerX
                    ]
                  <|
                    text <|
                        String.fromInt model.seconds
                ]
            ]
    }