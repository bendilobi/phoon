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
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.Session as Session exposing (BreathCount, BreathingSpeed, Session)
import Lib.Utils as Utils
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
    { settingsItemShown : SettingsItem }


init : () -> ( Model, Effect Msg )
init () =
    ( { settingsItemShown = NoItem }
    , Effect.none
    )



-- UPDATE


type SettingsItem
    = NoItem
    | Cycles
    | BreathingSpeed
    | BreathCount
    | RelaxRetDuration


type Msg
    = ReloadApp
    | DefaultCyclesChanged Int
    | DefaultRelaxRetDurationChanged Int
    | DefaultBreathingSpeedChanged BreathingSpeed
    | DefaultBreathCountChanged BreathCount
    | SettingsItemShown SettingsItem
    | ResetSettingItemStatus
    | ResetSettings


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

        ResetSettings ->
            ( model
            , Effect.updateSessionSettings Session.defaultSettings
            )

        SettingsItemShown item ->
            ( { model | settingsItemShown = item }
            , Effect.none
            )

        ResetSettingItemStatus ->
            ( { model | settingsItemShown = NoItem }
            , Effect.none
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
            , viewSettings shared model
            , viewTechInfo shared
            ]
    }


viewIntroduction : Shared.Model -> Element Msg
viewIntroduction shared =
    column [ width fill, spacing 10, Font.center ]
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
        , row [ width fill, paddingEach { top = 15, bottom = 0, left = 0, right = 0 } ]
            [ el [ Font.size 13, alignBottom ] <| text "Version 0.5.37 \"MVP+\""
            , el [ width fill ] <|
                el [ alignRight ] <|
                    (Components.Button.new { onPress = Just ReloadApp, label = text "App neu laden" }
                        |> Components.Button.withInline
                        |> Components.Button.view shared.colorScheme
                    )
            ]
        ]


viewSettings : Shared.Model -> Model -> Element Msg
viewSettings shared model =
    let
        hPad =
            20

        lastItemAttrs =
            let
                vPad =
                    15
            in
            [ width fill
            , paddingEach { top = vPad, bottom = vPad, right = hPad, left = 0 }
            , alignBottom
            ]

        --TODO: ColorScheme ergänzen und Farben von dort nehmen
        itemAttrs =
            lastItemAttrs
                ++ [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                   , Border.color <| rgb 0.7 0.7 0.7
                   ]

        settingsAttrs =
            [ width fill
            , Border.rounded 10
            , BG.color <| rgb 0.9 0.9 0.9
            , paddingEach { left = hPad, right = 0, top = 0, bottom = 0 }
            ]

        activeItemLabel : String -> Element Msg
        activeItemLabel label =
            Components.Button.new
                { onPress = Just ResetSettingItemStatus
                , label = text label
                }
                |> Components.Button.withInline
                |> Components.Button.view shared.colorScheme
    in
    column [ width fill, spacing 10 ]
        [ row [ width fill, alignBottom ]
            [ el
                [ Font.bold
                , Font.size 20
                , Font.color <| CS.guideColor shared.colorScheme
                ]
              <|
                text "Übung anpassen"
            , el [ width fill ] none
            , el [ alignBottom ] <|
                (Components.Button.new
                    { onPress = Just ResetSettings
                    , label = text "Zurücksetzen"
                    }
                    |> Components.Button.withInline
                    |> Components.Button.view shared.colorScheme
                )
            ]
        , column settingsAttrs
            [ if model.settingsItemShown == Cycles then
                el lastItemAttrs <|
                    column [ width fill, spacing 20 ]
                        [ activeItemLabel "Gesamtablauf"
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
                        ]

              else
                let
                    cycleString =
                        if shared.sessionSettings.cycles == 1 then
                            " Runde"

                        else
                            " Runden"
                in
                viewSettingsItem
                    { attributes = lastItemAttrs
                    , label = "Gesamtablauf"
                    , value = (shared.sessionSettings.cycles |> String.fromInt) ++ cycleString
                    , item = Cycles
                    }
            ]
        , el [ height <| px 15 ] none
        , column
            settingsAttrs
            [ if model.settingsItemShown == BreathingSpeed then
                el itemAttrs <|
                    column [ width fill, spacing 20 ]
                        [ activeItemLabel "Atemgeschwindigkeit"
                        , RadioGroup.new
                            { choices = Session.breathingSpeeds
                            , toString = Session.breathingSpeedDE
                            , onSelect = DefaultBreathingSpeedChanged
                            }
                            |> RadioGroup.withSelected shared.sessionSettings.breathingSpeed
                            |> RadioGroup.view shared.colorScheme
                        ]

              else
                viewSettingsItem
                    { label = "Atemgeschwindigkeit"
                    , value = Session.breathingSpeedDE shared.sessionSettings.breathingSpeed
                    , attributes = itemAttrs
                    , item = BreathingSpeed
                    }
            , if model.settingsItemShown == BreathCount then
                el lastItemAttrs <|
                    column [ width fill, spacing 20 ]
                        [ activeItemLabel "Atemzüge"
                        , RadioGroup.new
                            { choices = Session.breathCountChoices
                            , toString = Session.breathCountInt >> String.fromInt
                            , onSelect = DefaultBreathCountChanged
                            }
                            |> RadioGroup.withSelected shared.sessionSettings.breathCount
                            |> RadioGroup.view shared.colorScheme
                        ]

              else
                viewSettingsItem
                    { label = "Atemzüge"
                    , value = shared.sessionSettings.breathCount |> Session.breathCountInt |> String.fromInt
                    , attributes = lastItemAttrs
                    , item = BreathCount
                    }
            ]
        , let
            seconds =
                Session.breathCountInt shared.sessionSettings.breathCount
                    * Session.speedToMillis shared.sessionSettings.breathingSpeed
                    * 2
                    // 1000
          in
          paragraph
            [ Font.size 13
            , paddingEach { bottom = 15, top = 0, left = hPad, right = 0 }
            ]
            [ text "Dauer der Atemphase: "
            , el
                [ Font.color <| CS.guideColor shared.colorScheme
                , Font.bold
                ]
              <|
                text <|
                    Utils.formatSeconds seconds
            , if seconds < 60 then
                text " Sekunden"

              else
                text " Minuten"
            ]
        , column settingsAttrs
            [ if model.settingsItemShown == RelaxRetDuration then
                el lastItemAttrs <|
                    column [ width fill, spacing 20 ]
                        [ activeItemLabel "Erholungsretention"
                        , IntCrementer.new
                            { label =
                                \n ->
                                    paragraph []
                                        [ if n < 10 then
                                            el [ transparent True ] <| text "1"

                                          else
                                            none
                                        , el [ Font.bold ] <| text <| String.fromInt n
                                        , text " Sekunden"
                                        ]
                            , onCrement = DefaultRelaxRetDurationChanged
                            }
                            |> IntCrementer.withMin 5
                            |> IntCrementer.withMax 30
                            |> IntCrementer.view shared.colorScheme shared.sessionSettings.relaxRetDuration
                        ]

              else
                viewSettingsItem
                    { label = "Erholungsretention"
                    , value = (shared.sessionSettings.relaxRetDuration |> String.fromInt) ++ " Sekunden"
                    , attributes = lastItemAttrs
                    , item = RelaxRetDuration
                    }
            ]
        , let
            duration =
                Session.new shared.sessionSettings
                    |> Session.estimatedDurationMillis
                    |> (\millis -> millis // 1000)
          in
          paragraph [ paddingEach { top = 15, bottom = 0, left = 0, right = 0 } ]
            [ text "Geschätzte Dauer der Übung: "
            , el
                [ Font.color <| CS.guideColor shared.colorScheme
                , Font.bold
                ]
              <|
                text <|
                    Utils.formatSeconds duration
            , text <|
                if duration < 3600 then
                    " Minuten"

                else
                    " Stunden"
            ]
        ]


viewSettingsItem :
    { item : SettingsItem
    , label : String
    , value : String
    , attributes : List (Attribute Msg)
    }
    -> Element Msg
viewSettingsItem { item, label, value, attributes } =
    button [ width fill ]
        { onPress = Just <| SettingsItemShown item
        , label =
            row attributes
                [ text label
                , el [ alignRight, Font.color <| rgb 0.5 0.5 0.5 ] <| text value
                ]
        }


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
