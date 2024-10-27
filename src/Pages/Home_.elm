module Pages.Home_ exposing (Model, Msg, page)

import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Layouts
import Layouts.BaseLayout
import Layouts.BaseLayout.MainNav
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData, previousStreak, streakInfo)
import Lib.PageFading exposing (Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Texts as Texts exposing (bullet, bulletParagraph)
import Maybe
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import String.Format
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
        { header = Just <| Texts.motivationHeading shared.appLanguage
        , headerIcon =
            case shared.motivationData of
                Nothing ->
                    Nothing

                _ ->
                    Just <| Layouts.BaseLayout.MainNav.viewHeaderButton FeatherIcons.helpCircle <| InfoWindowToggled Welcome
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
                            viewWelcomeInfo shared

                        Just motData ->
                            -- viewMotivationInfo shared motData
                            case model.infoContent of
                                Welcome ->
                                    viewWelcomeInfo shared

                                StreakInfo ->
                                    viewMotivationInfo shared motData
        }



-- INIT


type alias Model =
    { infoContent : InfoWindowContent }


type InfoWindowContent
    = Welcome
    | StreakInfo


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { infoContent = StreakInfo }
    , Effect.batch
        [ Effect.adjustToday
        , case shared.motivationData of
            Nothing ->
                case shared.updateState of
                    Shared.Model.NotUpdating ->
                        Effect.sendMsg <| InfoWindowToggled Welcome

                    _ ->
                        Effect.none

            Just _ ->
                Effect.none
        ]
    )



-- UPDATE


type Msg
    = InfoWindowToggled InfoWindowContent



-- | NoOp


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        InfoWindowToggled content ->
            ( { model | infoContent = content }
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
    { title = Texts.motivationHeading shared.appLanguage
    , attributes =
        CS.primaryMotivation shared.colorScheme
    , element =
        el
            [ width fill
            , height fill
            , Events.onClick <| InfoWindowToggled StreakInfo
            , Font.bold
            ]
        <|
            case shared.motivationData of
                Nothing ->
                    viewWelcome shared

                Just motData ->
                    viewMotivationData shared model motData
    }


viewWelcome : Shared.Model -> Element msg
viewWelcome shared =
    column
        [ width fill
        , centerY
        , Font.center
        , Font.size 25
        , moveUp 70
        , spacing 20
        ]
        [ el [ centerX ] <| text Texts.appName
        , paragraph [ Font.size 17 ] [ text <| Texts.appSlogan shared.appLanguage ]
        ]


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
                60

            else
                0
        , below <|
            paragraph
                [ width fill
                , Font.size 20
                , Font.center
                , paddingXY 20 30
                ]
            <|
                if not streakValid then
                    [ text <| Texts.daysSinceLastPractice shared.appLanguage daysSinceLastSession
                    , text <|
                        if daysSinceLastSession - 1 == 1 then
                            ""

                        else
                            "..."
                                ++ Texts.letsGo shared.appLanguage
                    ]

                else if remainingFreezes == 0 && daysSinceLastSession > 0 && MotivationData.streak motData > 1 then
                    -- Last freeze will be used up if no practice today
                    [ text <| Texts.practiceToday shared.appLanguage ]

                else
                    []
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


viewStreak : ColorScheme -> Int -> Int -> Bool -> Int -> Element msg
viewStreak colorScheme size freezes freezeInDanger streak =
    let
        ringSizes =
            --TODO: Maybe try a different implementation, using width fill and padding to arrange the rings?
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


viewWelcomeInfo : Shared.Model -> Layouts.BaseLayout.Overlay Msg
viewWelcomeInfo shared =
    Layouts.BaseLayout.InfoWindow
        { header = Texts.welcome shared.appLanguage
        , info =
            column [ spacing 15 ]
                (Texts.introduction shared.appLanguage
                    ++ (case shared.standalone of
                            Just False ->
                                paragraph [ Border.rounded 20, Border.width 1, padding 10 ] <|
                                    Texts.installInstruction shared.appLanguage <|
                                        (FeatherIcons.share
                                            |> FeatherIcons.toHtml []
                                            |> html
                                        )

                            _ ->
                                none
                       )
                    :: Texts.introduction2 shared.appLanguage
                )
        , onClose = InfoWindowToggled Welcome
        }


viewMotivationInfo : Shared.Model -> MotivationData.MotivationData -> Layouts.BaseLayout.Overlay Msg
viewMotivationInfo shared motData =
    let
        { sessionsUntilNextFreeze, daysSinceLastSession, remainingFreezes, streakValid } =
            MotivationData.streakInfo shared.today shared.sessionSettings.practiceFrequencyTarget motData
    in
    Layouts.BaseLayout.InfoWindow
        { header = Texts.streakInfoHeader shared.appLanguage
        , info =
            column
                [ width fill
                , spacing 20
                ]
                [ row
                    [ width fill ]
                    [ viewKPI (Texts.lastStreak shared.appLanguage) <|
                        if streakValid then
                            MotivationData.previousStreak motData

                        else
                            Just <| MotivationData.streak motData
                    , el [ width fill ] none
                    , viewKPI (Texts.longestStreak shared.appLanguage) <| Just <| MotivationData.maxStreak motData
                    , el [ width fill ] none
                    , viewKPI (Texts.currentStreak shared.appLanguage) <|
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
                    bullet
                        (Texts.practiceUntilLongest shared.appLanguage diffToMaxStreak
                         -- |> String.Format.value (diffToMaxStreak |> String.fromInt)
                        )

                  else if diffToMaxStreak == 0 then
                    bullet <| Texts.caughtUpLongest shared.appLanguage

                  else
                    none
                , let
                    prevStreak =
                        if streakValid then
                            MotivationData.previousStreak motData

                        else
                            Just <| MotivationData.streak motData
                  in
                  case prevStreak of
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
                                bullet
                                    (Texts.practiceUntilLast shared.appLanguage diffToPreviousStreak
                                     -- |> String.Format.value (diffToPreviousStreak |> String.fromInt)
                                    )

                            else if diffToPreviousStreak == 0 then
                                bullet <| Texts.caughtUpLast shared.appLanguage

                            else
                                none
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
                    bulletParagraph <|
                        if daysUntilStreakEnd == 1 then
                            Texts.practiceTomorrow shared.appLanguage

                        else
                            Texts.practiceUntilWeekday shared.appLanguage
                                (shared.today
                                    |> Date.add Date.Days daysUntilStreakEnd
                                    |> Date.weekday
                                )
                                (daysUntilStreakEnd > 6)

                  else
                    none
                , case sessionsUntilNextFreeze of
                    Nothing ->
                        none

                    Just sessions ->
                        if remainingFreezes == MotivationData.maxStreakFreezes then
                            bullet <| Texts.maxRingsReached shared.appLanguage MotivationData.maxStreakFreezes

                        else
                            bullet <| Texts.nextRingAfter shared.appLanguage sessions
                , if streakValid && daysSinceLastSession > 1 then
                    bullet
                        (Texts.lastPracticeWas shared.appLanguage daysSinceLastSession)

                  else
                    none
                ]
        , onClose = InfoWindowToggled StreakInfo
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
                [--TODO: Save the initial Target of maxStreak and lastStreak and show it here:
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
