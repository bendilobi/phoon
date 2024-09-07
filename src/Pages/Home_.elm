module Pages.Home_ exposing (Model, Msg, page)

import Components.Dialog as Dialog
import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons exposing (alignCenter)
import Layouts
import Layouts.MainNav
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
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.MainNav
        { header = Just "Motivation finden"
        , enableScrolling = False
        , fadeOut = NoFade
        , overlay =
            if model.debugInfoHidden then
                Layouts.MainNav.NoOverlay

            else
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
                Layouts.MainNav.InfoWindow
                    { header = "Serie"
                    , info =
                        -- (Dialog.new
                        --     { header = "Infos zur Serie:"
                        --     , message =
                        paragraph
                            []
                            [ text "Freezes: "
                            , text <|
                                case shared.motivationData of
                                    Nothing ->
                                        ""

                                    Just data ->
                                        String.fromFloat <| MotivationData.streakFreezeDays data
                            , text "; Tage seit letzter Sitzung: "
                            , text <| String.fromInt daysSinceLastSession
                            , text "; Übungsziel: "
                            , text <| String.fromInt <| shared.sessionSettings.practiceFrequencyTarget
                            , text <| "; Übungsziel zu Beginn: "
                            , text <| initialTarget
                            ]
                    , onClose = DebugInfoToggled
                    }

        --     , choices =
        --         [ Dialog.choice
        --             { label = "Ok"
        --             , onChoose = DebugInfoToggled
        --             }
        --         ]
        --     }
        --     |> Dialog.withWidth (shrink |> maximum (shared.deviceInfo.window.width * 0.8 |> round))
        --     |> Dialog.view shared.colorScheme
        -- )
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



-- | NoOp


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



-- NoOp ->
--     ( model, Effect.none )
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
        -- viewMotivationData model shared.deviceInfo shared.today shared.motivationData shared.colorScheme
        viewMotivationData shared model
    }



-- viewMotivationData : Model -> Utils.Device -> Date.Date -> Maybe MotivationData -> ColorScheme -> Element Msg
-- viewMotivationData model deviceInfo today motData colorScheme =


viewMotivationData : Shared.Model -> Model -> Element Msg
viewMotivationData shared model =
    --TODO: Diese ganze Funktion übersichtlicher aufbauen -> case für Motivationdata ganz am Anfang, etc.
    let
        daysSinceLastSession =
            shared.motivationData
                |> Maybe.map MotivationData.lastSessionDate
                |> Maybe.andThen (\date -> Just <| Date.diff Date.Days date shared.today)
                |> Maybe.withDefault 0

        seriesContinued =
            ((daysSinceLastSession - (floor <| Maybe.withDefault 0 <| Maybe.map MotivationData.streakFreezeDays shared.motivationData))
                < 2
            )
                && ((shared.motivationData |> Maybe.map MotivationData.streakInitialTarget |> Maybe.withDefault 4)
                        <= shared.sessionSettings.practiceFrequencyTarget
                   )
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
                                shared.deviceInfo.window

                            streakWidgetSize =
                                min window.width window.height
                                    * 0.8
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
                                if not seriesContinued then
                                    if daysSinceLastSession - 1 == 1 then
                                        text <| "Tage seit letzter Übung"

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

        -- , paragraph
        --     [ width fill
        --     -- , Font.bold
        --     -- , Font.size 20
        --     , Font.center
        --     , transparent model.debugInfoHidden
        --     ]
        --     [ text "Freezes: "
        --     , text <|
        --         case motData of
        --             Nothing ->
        --                 ""
        --             Just data ->
        --                 String.fromFloat <| MotivationData.streakFreezeDays data
        --     , text "; Tage seit letzter Sitzung: "
        --     , text <| String.fromInt daysSinceLastSession
        --     ]
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
