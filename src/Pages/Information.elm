module Pages.Information exposing (Model, Msg, page)

import Browser.Navigation
import Components.Button
import Components.IntCrementer as IntCrementer
import Components.RadioGroup as RadioGroup
import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.Session as Session exposing (BreathCount, BreathingSpeed, Session)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav { header = Just "Übung optimieren" }



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
    = ReloadApp
    | DefaultCyclesChanged Int
    | DefaultRelaxRetDurationChanged Int
    | DefaultBreathingSpeedChanged BreathingSpeed
    | DefaultBreathCountChanged BreathCount


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ReloadApp ->
            ( model
            , Effect.sendCmd Browser.Navigation.reload
              --AndSkipCache
            )

        DefaultCyclesChanged cycles ->
            let
                settings =
                    shared.sessionSettings
            in
            ( model
            , Effect.updateSessionSettings { settings | cycles = cycles }
            )

        DefaultRelaxRetDurationChanged seconds ->
            let
                settings =
                    shared.sessionSettings
            in
            ( model
            , Effect.updateSessionSettings { settings | relaxRetDuration = seconds }
            )

        DefaultBreathingSpeedChanged speed ->
            let
                settings =
                    shared.sessionSettings
            in
            ( model
            , Effect.updateSessionSettings { settings | breathingSpeed = speed }
            )

        DefaultBreathCountChanged breathCount ->
            let
                settings =
                    shared.sessionSettings
            in
            ( model
            , Effect.updateSessionSettings { settings | breathCount = breathCount }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Information"
    , attributes =
        CS.primaryInformation shared.colorScheme
    , element =
        column
            [ width fill
            , spacing 60
            , paddingXY 20 30
            , Font.size 15
            ]
            [ viewIntroduction shared
            , viewSettings shared
            , viewTechInfo shared
            ]
    }


viewIntroduction : Shared.Model -> Element Msg
viewIntroduction shared =
    column [ width fill, spacing 10 ]
        [ paragraph
            [ Font.size 25
            , Font.bold
            , Font.color <| CS.guideColor shared.colorScheme
            ]
            [ text "Zoff - Wim Hof Atmung mit dem Hauch von Zen" ]

        -- TODO: Version im service-worker setzen und irgendwie per Javascript über Flags hierher bringen
        , paragraph [] [ text """
        Mit Zoff machst Du Deine Atemübung ganz entspannt, vielleicht sogar im Liegen und mit geschlossenen
        Augen - Klänge leiten Dich jeweils zum nächsten Schritt. Und wenn Du selbst entscheiden möchtest, wann es 
        weitergeht (z.B. Beginn und Ende der Retention), tippst Du einfach mit zwei Fingern irgendwo auf den Bildschirm.
        """ ]
        , row [ width fill, paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
            [ el [ Font.size 13, alignBottom ] <| text "Version 0.4.15 \"Mr. Flexible\""
            , el [ width fill ] <|
                el [ alignRight ] <|
                    (Components.Button.new { onPress = Just ReloadApp, label = text "App neu laden" }
                        |> Components.Button.withInline True
                        |> Components.Button.view shared.colorScheme
                    )
            ]
        ]


viewSettings : Shared.Model -> Element Msg
viewSettings shared =
    let
        setHead withPadding =
            let
                pad =
                    if withPadding then
                        15

                    else
                        0
            in
            [ paddingEach { top = pad, bottom = 0, left = 0, right = 0 }
            , Font.size 15
            ]

        settingsHeadingTop =
            setHead False

        settingsHeading =
            setHead True
    in
    column [ width fill, spacing 20 ]
        [ el
            [ Font.bold
            , Font.size 20
            , Font.color <| CS.guideColor shared.colorScheme
            ]
          <|
            text "Übung anpassen"
        , el settingsHeadingTop <| text "Anzahl Runden"
        , IntCrementer.new
            { label =
                \n ->
                    paragraph []
                        [ el [ Font.bold ] <| text <| String.fromInt n
                        , text " Runde"
                        , el [ transparent <| n == 1 ] <| text "n"
                        ]
            , onCrement = DefaultCyclesChanged
            }
            |> IntCrementer.withMin 1
            |> IntCrementer.withMax 9
            |> IntCrementer.view shared.colorScheme shared.sessionSettings.cycles
        , el settingsHeading <| text "Atemgeschwindigkeit"
        , RadioGroup.new
            { choices = Session.breathingSpeeds
            , toString = Session.breathingSpeedDE
            , onSelect = DefaultBreathingSpeedChanged
            }
            |> RadioGroup.withSelected shared.sessionSettings.breathingSpeed
            |> RadioGroup.view shared.colorScheme
        , el settingsHeading <| text "Anzahl Atemzüge"
        , RadioGroup.new
            { choices = Session.breathCountChoices
            , toString = Session.breathCountInt >> String.fromInt
            , onSelect = DefaultBreathCountChanged
            }
            |> RadioGroup.withSelected shared.sessionSettings.breathCount
            |> RadioGroup.view shared.colorScheme
        , el settingsHeading <|
            text "Dauer der Entspannungsretention"
        , IntCrementer.new
            { label =
                \n ->
                    paragraph []
                        [ el [ Font.bold ] <| text <| String.fromInt n
                        , text " Sekunden"
                        ]
            , onCrement = DefaultRelaxRetDurationChanged
            }
            |> IntCrementer.withMin 5
            |> IntCrementer.withMax 30
            |> IntCrementer.view shared.colorScheme shared.sessionSettings.relaxRetDuration
        ]


viewTechInfo : Shared.Model -> Element msg
viewTechInfo shared =
    el [] <|
        case MotivationData.getMotivationData shared.motivationData of
            Nothing ->
                text "Noch keine Motivationsdaten vorhanden"

            Just data ->
                column [ spacing 5, Font.size 13 ]
                    [ el
                        [ Font.bold
                        , Font.size 15
                        , Font.color <| CS.guideColor shared.colorScheme
                        ]
                      <|
                        text "Aktuell gespeicherte Motivationsdaten"
                    , text <| "Serie: " ++ String.fromInt data.series
                    , text <| "Letzte Sitzung: " ++ Date.toIsoString data.lastSessionDate
                    , text <| "Mittlere Ret: " ++ (String.join "," <| List.map String.fromInt data.meanRetentiontimes)
                    , text <| "Max Ret: " ++ String.fromInt data.maxRetention
                    ]
