module Pages.Information exposing (Model, Msg, page)

import Browser.Navigation
import Components.Button
import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.MotivationData as MotivationData exposing (MotivationData)
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
    = ReloadApp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReloadApp ->
            ( model
            , Effect.sendCmd Browser.Navigation.reload
              --AndSkipCache
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Information"
    , attributes =
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb 1 1 1
        ]
    , element =
        textColumn
            [ width fill
            , spacing 50
            , paddingXY 30 50
            ]
            [ paragraph [ Font.size 30, Font.bold ] [ text "Zoff - Wim Hoff Atmung mit dem Hauch von Zen" ]

            -- TODO: Version im service-worker setzen und irgendwie per Javascript über Flags hierher bringen
            , text "Version 0.3.23 \"Der Motivator\""
            , Components.Button.new { onPress = Just ReloadApp, label = text "App neu laden" }
                |> Components.Button.view
            , el [] <|
                case MotivationData.getMotivationData shared.motivationData of
                    Nothing ->
                        text "Noch keine Motivationsdaten vorhanden"

                    Just data ->
                        column [ spacing 10 ]
                            [ el [ Font.bold, Font.size 20 ] <| text "Aktuell gespeicherte Motivationsdaten"
                            , text <| "Serie: " ++ String.fromInt data.series
                            , text <| "Letzte Sitzung: " ++ Date.toIsoString data.lastSessionDate
                            , text <| "Mittlere Ret: " ++ (String.join "," <| List.map String.fromInt data.meanRetentiontimes)
                            , text <| "Max Ret: " ++ String.fromInt data.maxRetention
                            ]
            ]
    }
