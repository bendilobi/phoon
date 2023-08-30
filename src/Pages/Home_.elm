module Pages.Home_ exposing (Model, Msg, page)

import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.MotivationData as MotivationData exposing (MotivationData)
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
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Motivation"
    , attributes =
        [ BG.color <| rgb255 82 155 178
        , Font.color <| rgb255 255 253 231
        ]
    , element =
        textColumn
            [ width fill
            , paddingXY 30 50
            , spacing 40
            ]
            [ paragraph [ Font.size 30, Font.bold ] [ text "Ergebnisse der letzten Sitzung" ]
            , el [ centerX ] <|
                if List.length (SessionResults.getRetentionTimes shared.results) > 0 then
                    Utils.viewRetentionTimes <|
                        SessionResults.getRetentionTimes shared.results

                else
                    text "Aktuell keine Ergebnisse gespeichert"
            , el [] <|
                case MotivationData.getMotivationData shared.motivationData of
                    Nothing ->
                        text "Noch keine Motivationsdaten vorhanden"

                    Just data ->
                        column [ spacing 10 ]
                            [ text <| "Serie: " ++ String.fromInt data.series
                            , text <| "Letzte Sitzung: " ++ Date.toIsoString data.lastSessionDate
                            , text <| "Mittlere Ret: " ++ (String.join "," <| List.map String.fromInt data.meanRetentiontimes)
                            , text <| "Max Ret: " ++ String.fromInt data.maxRetention

                            -- , text <|
                            --     "Test: "
                            --         ++ (Date.add Date.Days -1 data.lastSessionDate
                            --                 -- |> Date.toIsoString
                            --                 |> Date.diff Date.Days data.lastSessionDate
                            --                 |> String.fromInt
                            --            )
                            ]
            ]
    }
