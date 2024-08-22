module Pages.Home_ exposing (Model, Msg, page)

import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons exposing (alignCenter)
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading exposing (Trigger(..))
import Lib.Utils as Utils
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
        viewMotivationData model shared.deviceInfo shared.today shared.motivationData shared.colorScheme
    }


viewMotivationData : Model -> Utils.Device -> Date.Date -> Maybe MotivationData -> ColorScheme -> Element Msg
viewMotivationData model deviceInfo today motData colorScheme =
    let
        daysSinceLastSession =
            motData
                |> Maybe.map MotivationData.lastSessionDate
                |> Maybe.andThen (\date -> Just <| Date.diff Date.Days date today)
                |> Maybe.withDefault 0

        seriesContinued =
            (daysSinceLastSession - (floor <| Maybe.withDefault 0 <| Maybe.map MotivationData.streakFreezeDays motData)) < 2

        -- seriesColor =
        --     if seriesContinued then
        --         CS.seriesGoodColor colorScheme
        --     else
        --         CS.seriesBadColor colorScheme
    in
    column
        [ width fill
        , padding 20
        , spacing 20
        , centerY
        ]
        [ case motData of
            Nothing ->
                none

            Just data ->
                el
                    [ width fill
                    , Events.onClick DebugInfoToggled
                    ]
                <|
                    --     el
                    --         [ width <| px 200
                    --         , height <| px 200
                    --         , Border.rounded 100
                    --         , Border.color <| rgb 1 1 1
                    --         , Border.width 7
                    --         , centerX
                    --         ]
                    --     <|
                    --         el
                    --             [ width <| px 160
                    --             , height <| px 160
                    --             , Font.center
                    --             , Font.size 70
                    --             , Font.color seriesColor
                    --             , Font.bold
                    --             , Border.rounded 80
                    --             , Border.color <| rgb 1 1 1
                    --             , Border.width 7
                    --             , centerX
                    --             , centerY
                    --             ]
                    --         <|
                    --             el [ centerX, centerY ] <|
                    --                 text <|
                    --                     if seriesContinued then
                    --                         String.fromInt <| MotivationData.series data
                    --                     else
                    --                         String.fromInt <| daysSinceLastSession - 1
                    if seriesContinued then
                        let
                            remainingFreezes =
                                MotivationData.streakFreezeDays data
                                    |> floor
                                    |> (\freezes ->
                                            if daysSinceLastSession > 1 then
                                                freezes - (daysSinceLastSession - 1)

                                            else
                                                freezes
                                       )

                            window =
                                deviceInfo.window

                            streakWidgetSize =
                                min window.width window.height
                                    * 0.8
                                    |> round
                        in
                        viewStreak
                            colorScheme
                            streakWidgetSize
                            remainingFreezes
                            (daysSinceLastSession > 0)
                        <|
                            MotivationData.series data

                    else
                        el
                            [ centerX
                            , centerY
                            , Font.color <| CS.seriesBadColor colorScheme
                            , Font.size 70
                            , Font.bold
                            , paddingXY 0 50
                            ]
                        <|
                            text <|
                                String.fromInt <|
                                    daysSinceLastSession

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
            [ case motData of
                Nothing ->
                    text <| "Willkommen bei Zoff!!"

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
                                        --TODO: Das gibts jetzt eigentlich nicht mehr, oder?
                                        text <| "Tag seit letzter Übung"

                                    else
                                        text <| "Tage seit letzter Übung... Auf geht's!"

                                else if freezes < 0 then
                                    text <| "0 Freezes übrig"

                                else if freezes == 0 && daysSinceLastSession > 0 && MotivationData.series data > 1 then
                                    -- Last freeze will be used up if no practice today
                                    text <| "Praktiziere noch heute, um Deinen Streak zu erhalten!"

                                else
                                    -- String.fromInt freezes ++ " Freezes übrig"
                                    none
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


viewStreak : ColorScheme -> Int -> Int -> Bool -> Int -> Element msg
viewStreak colorScheme size freezes freezeInDanger streak =
    let
        ringSizes =
            List.foldl (\ringNumber sizes -> size - (ringNumber * 20) :: sizes) [] <|
                List.range 0 (freezes - 1)

        viewRing : Int -> Element msg -> Element msg
        viewRing ringSize content =
            el
                [ centerX
                , centerY
                , width <| px ringSize
                , height <| px ringSize
                , Border.rounded <| ringSize // 2
                , Border.color <|
                    -- if ringSize == size && freezeInDanger then
                    --     rgb255 167 170 189
                    --     -- interactInactive
                    -- else
                    CS.primaryMotivationCopyColor colorScheme
                , Border.width <|
                    if ringSize == size && freezeInDanger then
                        1

                    else
                        3

                -- , if ringSize == size && freezeInDanger then
                --     Border.dashed
                --   else
                --     Border.solid
                ]
                content

        viewStreakNumber =
            el
                [ centerX
                , centerY

                --TODO: Berechnen aus Freezes-Anzahl, d.h. proportional zur Menge der Ringe?
                , Font.size 70
                , Font.bold
                , Font.color <| CS.seriesGoodColor colorScheme
                ]
            <|
                text <|
                    String.fromInt streak
    in
    List.foldl viewRing viewStreakNumber ringSizes
