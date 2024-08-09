module Pages.Home_ exposing (Model, Msg, page)

import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading exposing (Trigger(..))
import Maybe
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
    { debugInfoHidden : Bool }


init : () -> ( Model, Effect Msg )
init () =
    ( { debugInfoHidden = True }
    , Effect.sendCmd <| Task.perform TodayIs Date.today
    )



-- UPDATE


type Msg
    = TodayIs Date.Date
    | DebugInfoToggled


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        TodayIs date ->
            ( model
            , Effect.adjustToday date
            )

        DebugInfoToggled ->
            ( { model | debugInfoHidden = not model.debugInfoHidden }
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
        CS.primaryMotivation shared.colorScheme
    , element =
        viewMotivationData model shared.today shared.motivationData shared.colorScheme
    }


viewMotivationData : Model -> Date.Date -> Maybe MotivationData -> ColorScheme -> Element Msg
viewMotivationData model today motData colorScheme =
    let
        daysSinceLastSession =
            motData
                |> Maybe.map MotivationData.lastSessionDate
                |> Maybe.andThen (\date -> Just <| Date.diff Date.Days date today)
                |> Maybe.withDefault 0

        seriesContinued =
            daysSinceLastSession - (floor <| Maybe.withDefault 0 <| Maybe.map MotivationData.streakFreezeDays motData) < 2

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

        -- , paragraph
        --     [ width fill
        --     , Font.bold
        --     , Font.size 20
        --     , Font.center
        --     ]
        --     [ text <|
        --         case motData of
        --             Nothing ->
        --                 "Keine Motivationsdaten gespeichert"
        --             Just data ->
        --                 if seriesContinued then
        --                     if MotivationData.series data == 1 then
        --                         "...Tag praktiziert!"
        --                     else
        --                         "...Tage durchgehend praktiziert! Super!"
        --                 else if daysSinceLastSession - 1 == 1 then
        --                     "...Tag ausgelassen..."
        --                 else
        --                     "...Tage ausgelassen... hm..."
        --     ]
        , paragraph
            [ width fill
            , Font.bold
            , Font.size 20
            , Font.center
            , Events.onClick DebugInfoToggled
            ]
            [ text <|
                case motData of
                    Nothing ->
                        ""

                    Just data ->
                        MotivationData.streakFreezeDays data
                            |> floor
                            |> (\freezes ->
                                    if daysSinceLastSession > 1 then
                                        freezes - (daysSinceLastSession - 1)

                                    else
                                        freezes
                               )
                            |> (\freezes ->
                                    if not seriesContinued then
                                        if daysSinceLastSession - 1 == 1 then
                                            "Tag seit letzter Serie"

                                        else
                                            "Tage seit letzter Serie... Auf geht's!"

                                    else if freezes < 0 then
                                        "0 Freezes übrig"

                                    else if freezes == 0 && daysSinceLastSession > 0 && MotivationData.series data > 1 then
                                        -- Last freeze will be used up if no practice today
                                        "Praktiziere noch heute, um Deinen Streak zu erhalten!"

                                    else
                                        String.fromInt freezes ++ " Freezes übrig"
                               )
            ]
        , paragraph
            [ width fill

            -- , Font.bold
            -- , Font.size 20
            , Font.center
            , transparent model.debugInfoHidden
            ]
            [ text "Freezes: "
            , text <|
                case motData of
                    Nothing ->
                        ""

                    Just data ->
                        String.fromFloat <| MotivationData.streakFreezeDays data
            , text "; Tage seit letzter Sitzung: "
            , text <| String.fromInt daysSinceLastSession
            ]
        ]
