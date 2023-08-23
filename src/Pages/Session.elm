module Pages.Session exposing (Model, Msg, page)

-- import Element.Input exposing (button)

import Components.Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Utils as Utils
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
            , Effect.batch
                [ Effect.resultsUpdated SessionResults.empty
                , Effect.navigate <|
                    Session.currentPath shared.session
                ]
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
                paragraph []
                    [ text "Geschätztes Ende: "
                    , viewEstimatedTime shared
                    , text " Uhr"
                    ]
            , el [ centerX, centerY ] <|
                text <|
                    "Geschätzte Dauer: "
                        ++ (Utils.formatSeconds <|
                                Session.estimatedDuration shared.session
                                    // 1000
                           )
                        ++ " Minuten"
            , el [ centerX, centerY ] <|
                Components.Button.view
                    { onPress = Just SessionStartPressed
                    , label = text "Los geht's!"
                    }
            ]
    }


viewEstimatedTime : Shared.Model -> Element msg
viewEstimatedTime shared =
    let
        estimate =
            shared.time
                |> Time.posixToMillis
                |> (+) (Session.estimatedDuration shared.session)
                |> Time.millisToPosix

        hour =
            String.fromInt <| Time.toHour shared.zone estimate

        minute =
            Time.toMinute shared.zone estimate
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    text <| hour ++ ":" ++ minute
