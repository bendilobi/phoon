module Pages.Home_ exposing (Model, Msg, page)

import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.MotivationData as MotivationData exposing (MotivationData, series)
import Lib.SessionResults as SessionResults
import Lib.Utils as Utils
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task
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
    { today : Date.Date }


init : () -> ( Model, Effect Msg )
init () =
    ( { today = Date.fromRataDie 0 }
    , Effect.sendCmd <| Task.perform TodayIs Date.today
    )



-- UPDATE


type Msg
    = TodayIs Date.Date


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        TodayIs date ->
            ( { model | today = date }
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
        column
            [ width fill
            , height fill
            , BG.color <| rgb255 82 155 178
            ]
            [ -- TODO: Die Titeldarstellung ins Layout auslagern?
              el
                [ width fill
                , Font.color <| rgb255 82 155 178
                , Font.center
                , Font.bold
                , BG.color <| rgb255 50 49 46
                , padding 10
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                , Border.color <| rgb255 34 33 31
                ]
              <|
                text "Motivation finden"
            , viewMotivationData model.today shared.motivationData
            ]
    }


viewMotivationData : Date.Date -> MotivationData -> Element msg
viewMotivationData today motData =
    let
        daysSinceLastSession =
            MotivationData.lastSessionDate motData
                |> Maybe.andThen (\date -> Just <| Date.diff Date.Days date today)
                |> Maybe.withDefault 0

        seriesContinued =
            daysSinceLastSession < 2

        seriesColor =
            if seriesContinued then
                rgb255 170 255 0

            else
                rgb255 184 37 17
    in
    column
        [ width fill
        , padding 20
        , spacing 70
        , centerY
        ]
        [ case MotivationData.series motData of
            Nothing ->
                none

            Just series ->
                el
                    [ width fill
                    , Font.center
                    , Font.size 70
                    , Font.color seriesColor
                    , Font.bold
                    ]
                <|
                    text <|
                        if seriesContinued then
                            String.fromInt series

                        else
                            String.fromInt <| daysSinceLastSession - 1
        , el
            [ centerX
            , Font.bold
            , Font.size 20
            ]
          <|
            text <|
                case MotivationData.series motData of
                    -- TODO: MotivationData so umbauen, dass nicht so viel mit Maybes hantiert
                    --       werden muss
                    Nothing ->
                        "Keine Motivationsdaten gespeichert"

                    Just series ->
                        if seriesContinued then
                            if series == 1 then
                                "...Tag durchgehend praktiziert!"

                            else
                                "...Tage durchgehend praktiziert! Super!"

                        else if daysSinceLastSession - 1 == 1 then
                            "...Tag ausgelassen..."

                        else
                            "...Tage ausgelassen... hm..."
        ]



-- [ paragraph [ Font.size 30, Font.bold ] [ text "Ergebnisse der letzten Sitzung" ]
-- , el [ centerX ] <|
--     if List.length (SessionResults.getRetentionTimes shared.results) > 0 then
--         Utils.viewRetentionTimes <|
--             SessionResults.getRetentionTimes shared.results
--     else
--         text "Aktuell keine Ergebnisse gespeichert"
-- ]
