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
import Lib.MotivationData as MotivationData exposing (MotivationData, previousStreak)
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
        , headerIcon = Nothing
        , enableScrolling = False
        , fadeOut = NoFade
        , subPage = Nothing
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
        , moveUp <|
            if shared.deviceInfo.orientation == Portrait then
                70

            else
                0
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

                  else if remainingFreezes == 0 && daysSinceLastSession > 0 && MotivationData.streak motData > 1 then
                    -- Last freeze will be used up if no practice today
                    text <| "Praktiziere noch heute, um Deine Serie fortzusetzen!"

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
                MotivationData.streak motData

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
        , moveUp 70
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
    in
    Layouts.BaseLayout.InfoWindow
        { header = "Serie"
        , info =
            column
                [ width fill
                , spacing 20
                ]
                [ row
                    [ width fill ]
                    [ viewKPI "Letzte Serie" <| MotivationData.previousStreak motData
                    , el [ width fill ] none
                    , viewKPI "Längste Serie" <| Just <| MotivationData.maxStreak motData
                    , el [ width fill ] none
                    , viewKPI "Aktuelle Serie" <|
                        if streakValid then
                            Just <| MotivationData.streak motData

                        else
                            Nothing
                    ]
                , let
                    diffToMaxStreak =
                        MotivationData.maxStreak motData
                            - (if streakValid then
                                MotivationData.streak motData

                               else
                                0
                              )
                  in
                  if diffToMaxStreak < 4 && diffToMaxStreak > 0 then
                    bullet <|
                        paragraph []
                            [ text "Übe noch "
                            , text <| String.fromInt diffToMaxStreak
                            , text " mal und Du hast Deine längste Serie eingeholt!"
                            ]

                  else if diffToMaxStreak == 0 then
                    --TODO: Besonderen Text, wenn der Serienrekord gerade eingestellt wurde...
                    --      Dafür muss ich aber die gespeicherten Daten erweitern, oder?
                    --      Oder doch nur direkt nach der Übung eine Meldung zeigen, beim
                    --      Neustart der App aber nicht mehr? => Wert im Shared.Model ...
                    bullet <|
                        paragraph []
                            [ el [ Font.bold ] <| text "Du hast gerade Deine längste Serie bisher! Super!!" ]

                  else
                    -- bullet <|
                    --     paragraph []
                    --         [ text "Längste Serie bisher: "
                    --         , text <| String.fromInt <| MotivationData.maxStreak motData
                    --         ]
                    none
                , case MotivationData.previousStreak motData of
                    Nothing ->
                        none

                    Just previousStreak ->
                        if previousStreak == MotivationData.maxStreak motData then
                            {- This case is handled above -}
                            none

                        else
                            let
                                diffToPreviousStreak =
                                    previousStreak
                                        - (if streakValid then
                                            MotivationData.streak motData

                                           else
                                            0
                                          )
                            in
                            if diffToPreviousStreak < 4 && diffToPreviousStreak > 0 then
                                bullet <|
                                    paragraph []
                                        [ text "Übe noch "
                                        , text <| String.fromInt diffToPreviousStreak
                                        , text " mal und Du hast Deine letzte Serie eingeholt!"
                                        ]

                            else if diffToPreviousStreak == 0 then
                                --TODO: Besonderen Text, wenn der Serienrekord gerade eingestellt wurde...
                                --      Dafür muss ich aber die gespeicherten Daten erweitern, oder?
                                --      Oder doch nur direkt nach der Übung eine Meldung zeigen, beim
                                --      Neustart der App aber nicht mehr? => Wert im Shared.Model ...
                                bullet <|
                                    paragraph []
                                        [ text "Du hast Deine letzte Serie eingeholt! Super!!" ]

                            else
                                -- bullet <|
                                --     paragraph []
                                --         [ text "Letzte Serie: "
                                --         , text <| String.fromInt previousStreak
                                --         ]
                                none

                -- , bullet <|
                --     let
                --         maxRet =
                --             MotivationData.maxRetention motData
                --     in
                --     paragraph []
                --         [ text "Bisher längste Retention: "
                --         , text <|
                --             Millis.toString False maxRet
                --         , if Millis.toSeconds maxRet < 60 then
                --             text " Sekunden"
                --           else
                --             text " Minuten"
                --         ]
                , if streakValid && not (daysSinceLastSession > 0 && remainingFreezes == 0) then
                    let
                        daysUntilStreakEnd =
                            remainingFreezes
                                + (if daysSinceLastSession == 0 then
                                    1

                                   else
                                    0
                                  )
                    in
                    bullet <|
                        paragraph [] <|
                            if daysUntilStreakEnd == 1 then
                                [ text "Um die Serie fortzusetzen, übe spätestens morgen wieder!" ]

                            else
                                [ text "Um die Serie fortzusetzen, übe spätestens am "
                                , shared.today
                                    |> Date.add Date.Days daysUntilStreakEnd
                                    |> Date.weekday
                                    |> Utils.weekdayToGerman
                                    |> text
                                    |> el [ Font.bold ]
                                , if daysUntilStreakEnd > 6 then
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
                , if streakValid && daysSinceLastSession > 1 then
                    bullet <|
                        paragraph []
                            [ text "Deine letzte Übung war vor "
                            , text <| String.fromInt daysSinceLastSession
                            , text " Tagen"
                            ]

                  else
                    none

                -- , bullet <|
                --     paragraph []
                --         [ text "Übungsziel: "
                --         , text <| String.fromInt <| shared.sessionSettings.practiceFrequencyTarget
                --         ]
                -- , bullet <|
                --     paragraph []
                --         [ text <| "Übungsziel zu Beginn: "
                --         , text <| initialTarget
                --         ]
                -- , bullet <|
                --     paragraph []
                --         [ text "Debug-Info: "
                --         , text "Freezes: "
                --         , text <|
                --             String.fromFloat <|
                --                 MotivationData.streakFreezes motData
                --         , text ""
                --         ]
                ]
        , onClose = DebugInfoToggled
        }


viewKPI : String -> Maybe Int -> Element msg
viewKPI caption kpi =
    column
        [ spacing 5
        , Border.rounded 13
        , Border.width 1
        , padding 10
        , width <| px 100
        ]
        [ el [ centerX, Font.size 27, Font.extraBold ] <|
            el
                [--TODO: initial Target für maxStreak und lastStreak speichern und hier anzeigen
                 -- onRight <| el [ Font.size 11 ] <| text "5"
                ]
            <|
                text <|
                    case kpi of
                        Nothing ->
                            "X"

                        Just n ->
                            String.fromInt n
        , paragraph [ width fill, Font.center, Font.size 11 ] [ text caption ]
        ]
