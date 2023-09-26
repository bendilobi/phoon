module Pages.Information exposing (Model, Msg, page)

import Browser.Navigation
import Components.Button
import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
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
        CS.primaryInformation shared.colorScheme
    , element =
        textColumn
            [ width fill
            , spacing 50
            , paddingXY 30 50
            ]
            [ paragraph
                [ Font.size 30
                , Font.bold
                , Font.color <| CS.guide shared.colorScheme
                ]
                [ text "Zoff - Wim Hof Atmung mit dem Hauch von Zen" ]

            -- TODO: Version im service-worker setzen und irgendwie per Javascript über Flags hierher bringen
            , text "Version 0.3.49 \"Sunrise\""
            , Components.Button.new { onPress = Just ReloadApp, label = text "App neu laden" }
                |> Components.Button.view shared.colorScheme
            , el [] <|
                case MotivationData.getMotivationData shared.motivationData of
                    Nothing ->
                        text "Noch keine Motivationsdaten vorhanden"

                    Just data ->
                        column [ spacing 10 ]
                            [ el
                                [ Font.bold
                                , Font.size 20
                                , Font.color <| CS.guide shared.colorScheme
                                ]
                              <|
                                text "Aktuell gespeicherte Motivationsdaten"
                            , text <| "Serie: " ++ String.fromInt data.series
                            , text <| "Letzte Sitzung: " ++ Date.toIsoString data.lastSessionDate
                            , text <| "Mittlere Ret: " ++ (String.join "," <| List.map String.fromInt data.meanRetentiontimes)
                            , text <| "Max Ret: " ++ String.fromInt data.maxRetention
                            ]
            ]
    }
