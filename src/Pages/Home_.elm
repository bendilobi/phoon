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
        { init = init shared
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
                    case shared.motivationData of
                        Nothing ->
                            viewWelcomeInfo

                        Just motData ->
                            viewMotivationInfo shared motData
        }



-- INIT


type alias Model =
    {}


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( {}
    , Effect.batch
        [ Effect.sendCmd <| Task.perform TodayIs Date.today
        , case shared.motivationData of
            Nothing ->
                case shared.updateState of
                    Shared.Model.NotUpdating ->
                        Effect.sendMsg DebugInfoToggled

                    _ ->
                        Effect.none

            Just _ ->
                Effect.none
        ]
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
        el
            [ width fill
            , height fill
            , Events.onClick DebugInfoToggled
            , Font.bold
            ]
        <|
            case shared.motivationData of
                Nothing ->
                    viewWelcome

                Just motData ->
                    viewMotivationData shared model motData
    }


viewMotivationData : Shared.Model -> Model -> MotivationData -> Element Msg
viewMotivationData shared model motData =
    let
        { streakValid, daysSinceLastSession, remainingFreezes } =
            MotivationData.streakInfo shared.today shared.sessionSettings.practiceFrequencyTarget motData
    in
    el
        [ width fill
        , centerY
        , Font.size 70
        , moveUp 40
        , below <|
            paragraph
                [ width fill
                , Font.size 20
                , Font.center
                , paddingXY 20 30
                ]
                [ if not streakValid then
                    if daysSinceLastSession - 1 == 1 then
                        text <| "Tage seit letzter Übung"

                    else
                        text <| "Tage seit letzter Übung... Auf geht's!"

                  else if remainingFreezes == 0 && daysSinceLastSession > 0 && MotivationData.series motData > 1 then
                    -- Last freeze will be used up if no practice today
                    text <| "Praktiziere noch heute, um Deinen Streak zu erhalten!"

                  else
                    none
                ]
        ]
    <|
        if streakValid then
            let
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
                MotivationData.series motData

        else
            el
                [ centerX
                , Font.color <| CS.seriesBadColor shared.colorScheme
                ]
            <|
                text <|
                    String.fromInt <|
                        daysSinceLastSession



-- ]


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
                , Font.color <| CS.seriesGoodColor colorScheme
                ]
            <|
                text <|
                    String.fromInt streak
    in
    List.foldl viewRing viewStreakNumber ringSizes


viewWelcome : Element msg
viewWelcome =
    paragraph
        [ width fill
        , centerY
        , Font.center
        , Font.size 25
        , moveUp 40
        ]
        [ text <| "Willkommen bei Zoff!!" ]


viewWelcomeInfo : Layouts.BaseLayout.Overlay Msg
viewWelcomeInfo =
    Layouts.BaseLayout.InfoWindow
        { header = "Herzlich willkommen bei Zoff"
        , info =
            column [ spacing 25 ]
                [ paragraph [] [ text """
                                    Mit Zoff machst Du Deine Atemübung ganz entspannt, vielleicht sogar im Liegen und mit geschlossenen
                                    Augen - Klänge leiten Dich jeweils zum nächsten Schritt. Und wenn Du selbst entscheiden möchtest, wann es 
                                    weitergeht (z.B. Beginn und Ende der Retention), tippst Du einfach mit drei Fingern irgendwo auf den Bildschirm.
                                    """ ]
                , paragraph [] [ text """
                                Zoff hilft Dir auch dabei, eine regelmäßige Übungspraxis aufrechtzuerhalten: Hier wird erfasst, wie oft Du in Serie
                                Die Atemübungen gemacht hast. Und unter "Optimieren" kannst Du festlegen, wie oft pro Woche Du üben willst - so 
                                kannst Du auch hier und dort mal einen Tag auslassen, ohne Deine Serie zu verlieren!
                                """ ]
                ]
        , onClose = DebugInfoToggled
        }


viewMotivationInfo : Shared.Model -> MotivationData.MotivationData -> Layouts.BaseLayout.Overlay Msg
viewMotivationInfo shared motData =
    let
        { sessionsUntilNextFreeze, daysSinceLastSession, remainingFreezes, streakValid } =
            MotivationData.streakInfo shared.today shared.sessionSettings.practiceFrequencyTarget motData

        initialTarget =
            shared.motivationData
                |> Maybe.map MotivationData.streakInitialTarget
                |> Maybe.withDefault 4
                |> String.fromInt
    in
    Layouts.BaseLayout.InfoWindow
        { header = "Serie"
        , info =
            column [ spacing 20 ]
                [ paragraph [] [ text "Informationen zur Serie, vorerst zu Debugging-Zwecken:" ]
                , column
                    [ spacing 20
                    , paddingXY 20 0
                    ]
                    [ bullet <|
                        paragraph []
                            [ text "Bisher längste Serie: "
                            , text <|
                                String.fromInt <|
                                    MotivationData.maxStreak motData
                            ]
                    , bullet <|
                        paragraph []
                            [ text "Bisher längste Retention: "
                            , text <|
                                Millis.toString True <|
                                    MotivationData.maxRetention motData
                            , text " Minuten"
                            ]
                    , bullet <|
                        paragraph []
                            [ text "Freezes: "
                            , text <|
                                String.fromFloat <|
                                    MotivationData.streakFreezes motData
                            ]
                    , if streakValid then
                        bullet <|
                            paragraph []
                                [ text "Um die Serie zu erhalten, übe spätestens am "
                                , shared.today
                                    |> Date.add Date.Days (remainingFreezes + 1)
                                    |> Date.weekday
                                    |> Utils.weekdayToGerman
                                    |> text
                                , if remainingFreezes > 7 then
                                    text " nächste Woche"

                                  else
                                    none
                                ]

                      else
                        none
                    , case sessionsUntilNextFreeze of
                        Nothing ->
                            none

                        Just sessions ->
                            bullet <|
                                paragraph []
                                    [ text "Nächster Ring kommt nach "
                                    , text <|
                                        if sessions > 1 then
                                            String.fromInt sessions ++ " Übungen"

                                        else
                                            "der nächsten Übung"
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
