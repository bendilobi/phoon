module Pages.Session exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Element.Input exposing (button)
import Layouts
import Lib.BreathingSession as BS
import Lib.SessionResults as SessionResults
import Page exposing (Page)
import Route exposing (Route)
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


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SessionStartPressed ->
            let
                newSession =
                    BS.createSession 4
            in
            ( model
            , Effect.batch
                [ Effect.sessionUpdated newSession
                , Effect.resultsUpdated <| SessionResults.empty
                , Effect.navigate <| BS.currentPath newSession
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Session"
    , attributes = []
    , element =
        column
            [ width fill
            , height fill
            , BG.color <| rgb255 200 196 183
            ]
            [ el [ height fill, width fill ] <|
                button
                    [ centerX
                    , centerY
                    , padding 20
                    , BG.color <| rgb255 161 158 147
                    ]
                    { onPress = Just SessionStartPressed
                    , label = text "Los geht's!"
                    }
            ]
    }
