module Pages.PrepareSession exposing (Model, Msg, page)

import Components.Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
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
                , Effect.playSound Utils.SessionStart
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
            [ el
                [ width fill
                , Font.color <| rgb255 200 196 183
                , Font.center
                , Font.bold
                , BG.color <| rgb255 50 49 46
                , padding 10
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 34 33 31
                ]
              <|
                text "Sitzung vorbereiten"
            , column
                [ width fill
                , padding 20
                , Font.center
                , spacing 30
                , centerX
                , centerY
                ]
                [ paragraph []
                    -- TODO: Entscheiden, wie ich in Session die Rundenanzahl repräsentieren
                    --       möchte und dann hier verwenden
                    [ el [ Font.bold ] <| text "4"
                    , text " Runden"
                    ]
                , paragraph []
                    [ text "Geschätztes Ende: "
                    , el [ Font.bold ] <| viewEstimatedTime shared
                    , text " Uhr"
                    ]

                -- , el [] <|
                --     text <|
                --         "Geschätzte Dauer: "
                --             ++ (Utils.formatSeconds <|
                --                     Session.estimatedDuration shared.session
                --                         // 1000
                --                )
                --             ++ " Minuten"
                , el [ width fill ] <|
                    Components.Button.view
                        { onPress = Just SessionStartPressed
                        , label = text "Los geht's!"
                        }
                ]
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
