module Pages.Home_ exposing (Model, Msg, page)

import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading exposing (Trigger(..))
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
    Layouts.MainNav
        { header = Just "Motivation finden"
        , enableScrolling = False
        , fadeOut = NoFade
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.sendCmd <| Task.perform TodayIs Date.today
    )



-- UPDATE


type Msg
    = TodayIs Date.Date


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        TodayIs date ->
            ( model
            , Effect.adjustToday date
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
        CS.primaryMotivation shared.colorScheme
    , element =
        viewMotivationData shared.today shared.motivationData shared.colorScheme
    }


viewMotivationData : Date.Date -> Maybe MotivationData -> ColorScheme -> Element msg
viewMotivationData today motData colorScheme =
    let
        daysSinceLastSession =
            motData
                |> Maybe.map MotivationData.lastSessionDate
                |> Maybe.andThen (\date -> Just <| Date.diff Date.Days date today)
                |> Maybe.withDefault 0

        seriesContinued =
            daysSinceLastSession < 2

        seriesColor =
            if seriesContinued then
                CS.seriesGoodColor colorScheme

            else
                CS.seriesBadColor colorScheme
    in
    column
        [ width fill
        , padding 20
        , spacing 70
        , centerY
        ]
        [ case motData of
            Nothing ->
                none

            Just data ->
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
                            String.fromInt <| MotivationData.series data

                        else
                            String.fromInt <| daysSinceLastSession - 1
        , paragraph
            [ width fill
            , Font.bold
            , Font.size 20
            , Font.center
            ]
            [ text <|
                case motData of
                    Nothing ->
                        "Keine Motivationsdaten gespeichert"

                    Just data ->
                        if seriesContinued then
                            if MotivationData.series data == 1 then
                                "...Tag praktiziert!"

                            else
                                "...Tage durchgehend praktiziert! Super!"

                        else if daysSinceLastSession - 1 == 1 then
                            "...Tag ausgelassen..."

                        else
                            "...Tage ausgelassen... hm..."
            ]
        ]
