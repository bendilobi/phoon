module Pages.Home_ exposing (Model, Msg, page)

import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Layouts
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading exposing (Trigger(..))
import Lib.SafeArea as SafeArea exposing (SafeArea)
import Lib.Utils as Utils exposing (bullet)
import Maybe
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Task
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.BaseLayout_MainNav
        { header = Just "Motivation finden"
        , enableScrolling = False
        , fadeOut = NoFade
        , overlay =
            case shared.infoWindowState of
                Shared.Model.Closed ->
                    Layouts.BaseLayout.NoOverlay

                _ ->
                    let
                        daysSinceLastSession =
                            shared.motivationData
                                |> Maybe.map MotivationData.lastSessionDate
                                |> Maybe.andThen (\date -> Just <| Date.diff Date.Days date shared.today)
                                |> Maybe.withDefault 0

                        initialTarget =
                            shared.motivationData
                                |> Maybe.map MotivationData.streakInitialTarget
                                |> Maybe.withDefault 4
                                |> String.fromInt
                    in
                    Layouts.BaseLayout.InfoWindow
                        { header = "Serie"
                        , info =
                            column [ spacing 20, Font.size 15 ]
                                [ paragraph [] [ text "Informationen zur Serie, vorerst zu Debugging-Zwecken:" ]
                                , column
                                    [ spacing 20
                                    , paddingXY 20 0
                                    ]
                                    [ bullet <|
                                        paragraph []
                                            [ text "Bisher längste Serie: "
                                            , text <|
                                                case shared.motivationData of
                                                    Nothing ->
                                                        ""

                                                    Just data ->
                                                        MotivationData.maxStreak data
                                                            |> String.fromInt
                                            ]
                                    , bullet <|
                                        paragraph []
                                            [ text "Bisher längste Retention: "
                                            , text <|
                                                case shared.motivationData of
                                                    Nothing ->
                                                        ""

                                                    Just data ->
                                                        MotivationData.maxRetention data
                                                            |> Millis.toString True
                                            , text " Minuten"
                                            ]
                                    , bullet <|
                                        paragraph []
                                            [ text "Freezes: "
                                            , text <|
                                                case shared.motivationData of
                                                    Nothing ->
                                                        ""

                                                    Just data ->
                                                        String.fromFloat <| MotivationData.streakFreezeDays data
                                            ]
                                    , bullet <|
                                        paragraph []
                                            [ text "Tage seit letzter Sitzung: "
                                            , text <| String.fromInt daysSinceLastSession
                                            ]
                                    , bullet <|
                                        paragraph []
                                            [ text "Übungsziel: "
                                            , text <| String.fromInt <| shared.sessionSettings.practiceFrequencyTarget
                                            ]
                                    , bullet <|
                                        paragraph []
                                            [ text <| "Übungsziel zu Beginn: "
                                            , text <| initialTarget
                                            ]
                                    ]
                                ]
                        , onClose = DebugInfoToggled
                        }
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
    | DebugInfoToggled



-- | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        TodayIs date ->
            ( model
            , Effect.adjustToday date
            )

        DebugInfoToggled ->
            ( model
            , case shared.infoWindowState of
                Shared.Model.Closed ->
                    Effect.setInfoWindowState Shared.Model.Half

                _ ->
                    Effect.setInfoWindowState Shared.Model.Closed
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
        viewMotivationData shared model
    }


viewMotivationData : Shared.Model -> Model -> Element Msg
viewMotivationData shared model =
    --TODO: Diese ganze Funktion übersichtlicher aufbauen -> case für Motivationdata ganz am Anfang, etc.
    let
        { streakValid, daysSinceLastSession } =
            case shared.motivationData of
                Nothing ->
                    { streakValid = False, daysSinceLastSession = 0 }

                Just motData ->
                    MotivationData.streakInfo shared.today shared.sessionSettings.practiceFrequencyTarget motData
    in
    column
        [ width fill
        , padding 20
        , spacing 20
        , centerY
        ]
        [ case shared.motivationData of
            Nothing ->
                none

            Just data ->
                el
                    [ width fill
                    , Events.onClick DebugInfoToggled
                    ]
                <|
                    if streakValid then
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
                                shared.deviceInfo.window

                            streakWidgetSize =
                                min window.width window.height
                                    |> (\size -> size - (SafeArea.maxX shared.safeAreaInset |> toFloat))
                                    |> (*) 0.8
                                    |> round
                        in
                        viewStreak
                            shared.colorScheme
                            streakWidgetSize
                            remainingFreezes
                            (daysSinceLastSession > 0)
                        <|
                            MotivationData.series data

                    else
                        el
                            [ centerX
                            , centerY
                            , Font.color <| CS.seriesBadColor shared.colorScheme
                            , Font.size 70
                            , Font.bold
                            , paddingXY 0 50
                            ]
                        <|
                            text <|
                                String.fromInt <|
                                    daysSinceLastSession
        , paragraph
            [ width fill
            , Font.bold
            , Font.size 20
            , Font.center
            , Events.onClick DebugInfoToggled
            ]
            [ case shared.motivationData of
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
                                if not streakValid then
                                    if daysSinceLastSession - 1 == 1 then
                                        text <| "Tage seit letzter Übung"

                                    else
                                        text <| "Tage seit letzter Übung... Auf geht's!"

                                else if freezes == 0 && daysSinceLastSession > 0 && MotivationData.series data > 1 then
                                    -- Last freeze will be used up if no practice today
                                    text <| "Praktiziere noch heute, um Deinen Streak zu erhalten!"

                                else
                                    -- String.fromInt freezes ++ " Freezes übrig"
                                    none
                           )
            ]
        ]


viewStreak : ColorScheme -> Int -> Int -> Bool -> Int -> Element msg
viewStreak colorScheme size freezes freezeInDanger streak =
    let
        ringSizes =
            --TODO: Statt statischen Größen auf "width fill" und entsprechendem Padding basieren?
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
                    CS.primaryMotivationCopyColor colorScheme
                , Border.width <|
                    if ringSize == (List.head ringSizes |> Maybe.withDefault 0) && freezeInDanger then
                        --TODO: In Kommentar erklären, warum inneren Ring nicht äußeren
                        1

                    else
                        3
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
