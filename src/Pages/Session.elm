module Pages.Session exposing (Model, Msg, page)

-- import Element.Input exposing (button)

import Components.Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.BreathingSession as BreathingSession
import Lib.SessionResults as SessionResults
import Lib.Utils as Utils
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav {}



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
            , Effect.navigate <| BreathingSession.currentPath shared.session
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Session"
    , attributes = []
    , element =
        column
            [ width fill
            , height fill
            , BG.color <| rgb255 200 196 183
            , spacing 50
            ]
            [ el [ centerX, centerY ] <|
                text <|
                    "Geschätzte Dauer: "
                        ++ (Utils.formatSeconds <|
                                BreathingSession.estimatedDuration shared.session
                           )
                        ++ " Minuten"
            , el [ centerX, centerY ] <|
                Components.Button.view
                    { onPress = Just SessionStartPressed
                    , label = text "Los geht's!"
                    }
            ]
    }
