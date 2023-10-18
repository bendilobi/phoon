module Pages.Information exposing (Model, Msg, page)

import Api
import Browser.Events
import Chart
import Chart.Attributes as ChartA
import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Components.Button
import Components.IntCrementer as IntCrementer
import Components.RadioGroup as RadioGroup
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Http
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.SafeArea as SafeArea
import Lib.Session as Session exposing (BreathCount, BreathingSpeed, Session)
import Lib.Utils as Utils
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Svg
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav { header = Just "Übung optimieren" }



-- INIT


type alias Model =
    { settingsItemShown : SettingsItem
    , bubble : Bubble.Model Msg
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { settingsItemShown = NoItem
      , bubble =
            Bubble.init
                { bubbleType = Bubble.Static
                , onFinished = Nothing
                }
      }
    , if shared.versionOnServer /= Api.Loading && not shared.justUpdated then
        Effect.checkVersion ReceivedNewestVersionString

      else
        Effect.none
    )



-- UPDATE


type SettingsItem
    = NoItem
    | Cycles
    | BreathingSpeed
    | BreathCount
    | RelaxRetDuration


type Msg
    = Tick Time.Posix
    | ReloadApp Bool
    | VisibilityChanged Browser.Events.Visibility
    | DefaultCyclesChanged Int
    | DefaultRelaxRetDurationChanged Int
    | DefaultBreathingSpeedChanged BreathingSpeed
    | DefaultBreathCountChanged BreathCount
    | SettingsItemShown SettingsItem
    | ResetSettingItemStatus
    | ResetSettings
    | ReceivedNewestVersionString (Result Http.Error String)


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            Bubble.update
                { msg = Bubble.Tick
                , model = model.bubble
                , toModel = \bubble -> { model | bubble = bubble }
                }

        VisibilityChanged visibility ->
            ( model
            , case visibility of
                Browser.Events.Hidden ->
                    Effect.none

                Browser.Events.Visible ->
                    Effect.checkVersion ReceivedNewestVersionString
            )

        ReceivedNewestVersionString response ->
            ( model, Effect.receivedVersionOnServer response )

        ReloadApp once ->
            ( model
            , if once then
                Effect.reload

              else
                Effect.updateApp
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
            let
                ( bModel, _ ) =
                    Bubble.update
                        { msg = Bubble.Reset
                        , model = model.bubble
                        , toModel = \bubble -> { model | bubble = bubble }
                        }
            in
            ( { model
                | settingsItemShown = item
                , bubble =
                    if item == BreathingSpeed then
                        bModel.bubble

                    else
                        model.bubble
              }
            , Effect.none
            )

        ResetSettingItemStatus ->
            ( { model | settingsItemShown = NoItem }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Sub.batch
        [ Browser.Events.onVisibilityChange VisibilityChanged
        , if model.settingsItemShown == BreathingSpeed then
            Time.every (Session.speedToMillis shared.sessionSettings.breathingSpeed |> toFloat) Tick

          else
            Sub.none
        ]



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

            -- , paddingXY 20 30
            , paddingEach { left = 20, right = 20, top = 30, bottom = 20 }
            , Font.size 15

            -- , inFront <|
            --     Dialog.view <|
            --         Just
            --             { closeMessage = Nothing --Just CloseDialog
            --             , maskAttributes = []
            --             , containerAttributes =
            --                 [ padding 10
            --                 , BG.color <| rgb 1 1 1
            --                 , centerX
            --                 , centerY
            --                 , Border.rounded 10
            --                 ]
            --             , headerAttributes =
            --                 [ Border.color <| rgb 0.9 0.9 0.9
            --                 , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            --                 ]
            --             , bodyAttributes = [ padding 30 ]
            --             , footerAttributes = []
            --             , header = Just <| text "Blah"
            --             , body = Just <| text "Sülz, laber..."
            --             , footer =
            --                 Just <|
            --                     (Components.Button.new
            --                         { onPress = Nothing
            --                         , label = text "OK..."
            --                         }
            --                         |> Components.Button.view shared.colorScheme
            --                     )
            --             }
            ]
            [ viewIntroduction shared model
            , viewUpdate shared model
            , viewRetentionTrend shared
            , viewSettings shared model
            , viewTechInfo shared model
            ]
    }


viewIntroduction : Shared.Model -> Model -> Element Msg
viewIntroduction shared model =
    column [ width fill, spacing 10, Font.center ]
        [ paragraph
            [ Font.size 25
            , Font.bold
            , Font.color <| CS.guideColor shared.colorScheme
            ]
            [ text "Zoff - Wim Hof Atmung mit dem Hauch von Zen" ]
        , paragraph [] [ text """
        Mit Zoff machst Du Deine Atemübung ganz entspannt, vielleicht sogar im Liegen und mit geschlossenen
        Augen - Klänge leiten Dich jeweils zum nächsten Schritt. Und wenn Du selbst entscheiden möchtest, wann es 
        weitergeht (z.B. Beginn und Ende der Retention), tippst Du einfach mit zwei Fingern irgendwo auf den Bildschirm.
        """ ]

        -- , text <|
        --     "Version on Server: "
        --         ++ (case shared.versionOnServer of
        --                 Api.Loading ->
        --                     "Loading..."
        --                 Api.Success version ->
        --                     version
        --                 Api.Failure err ->
        --                     Api.failureToString err
        --            )
        ]


viewUpdate : Shared.Model -> Model -> Element Msg
viewUpdate shared model =
    let
        serverResponse =
            case shared.versionOnServer of
                Api.Success versionString ->
                    Just versionString

                _ ->
                    Nothing
    in
    case serverResponse of
        Nothing ->
            none

        Just versionOnServer ->
            if shared.currentVersion /= versionOnServer then
                column [ width fill, spacing 10 ]
                    [ text <|
                        "Ein Update ist verfügbar von Version "
                            ++ shared.currentVersion
                            ++ " auf "
                            ++ versionOnServer
                    , Components.Button.new { onPress = Just <| ReloadApp False, label = text "Update jetzt laden" }
                        |> Components.Button.withLightColor
                        |> Components.Button.view shared.colorScheme
                    ]

            else
                none


viewRetentionTrend : Shared.Model -> Element msg
viewRetentionTrend shared =
    --TODO: Chart in eigene Komponente und dann Imports wie bei den Beispielen benennen
    case MotivationData.meanRetentionTimes shared.motivationData of
        Nothing ->
            none

        Just meanTimes ->
            if List.length meanTimes < 2 then
                none

            else
                let
                    data =
                        meanTimes
                            |> List.reverse
                            |> List.indexedMap (\i d -> { x = toFloat i, y = toFloat d })

                    max =
                        toFloat <| Maybe.withDefault 0 <| MotivationData.maxRetention shared.motivationData

                    paddingX =
                        115 + (SafeArea.maxX shared.safeAreaInset * 2)
                in
                column [ width fill, spacing 15 ]
                    [ el
                        [ Font.bold
                        , Font.size 20
                        , Font.color <| CS.guideColor shared.colorScheme
                        ]
                      <|
                        text "Retentionstrend"
                    , el
                        [ centerX
                        , width <| px <| shared.windowSize.width - paddingX
                        , height <| px 200
                        ]
                        (Chart.chart
                            [ ChartA.width <| toFloat <| shared.windowSize.width - paddingX
                            , ChartA.height 200
                            , ChartA.domain
                                [ ChartA.lowest 0 ChartA.orLower
                                , ChartA.highest (max + (max / 7)) ChartA.orHigher
                                ]
                            ]
                            [ Chart.generate (List.length meanTimes)
                                Chart.ints
                                .x
                                []
                              <|
                                \plane int ->
                                    [ Chart.xTick
                                        [ ChartA.x (toFloat int)
                                        , ChartA.color <| CS.interactInactiveDarkerColorHex shared.colorScheme
                                        , ChartA.noGrid
                                        ]
                                    ]
                            , Chart.series .x
                                [ Chart.interpolated .y
                                    [ ChartA.opacity 0.6
                                    , ChartA.gradient []
                                    , ChartA.monotone
                                    , ChartA.color <| CS.guideColorHex shared.colorScheme
                                    ]
                                    []
                                ]
                                data
                            , Chart.withPlane <|
                                \p ->
                                    [ Chart.line
                                        [ ChartA.x1 p.x.min
                                        , ChartA.y1 max
                                        , ChartA.x2 p.x.max
                                        , ChartA.color <| CS.seriesGoodColorHex shared.colorScheme
                                        , ChartA.width 2
                                        ]
                                    ]
                            , Chart.yLabel
                                [ ChartA.x 0
                                , ChartA.y max
                                , ChartA.color <| CS.interactInactiveDarkerColorHex shared.colorScheme
                                ]
                                [ Svg.text <| Utils.formatSeconds <| round max ]
                            ]
                            |> html
                        )
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

        itemAttrs =
            lastItemAttrs
                ++ [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                   , Border.color <| CS.settingsDarkerColor shared.colorScheme
                   ]

        settingsAttrs =
            [ width fill
            , Border.rounded 10
            , BG.color <| CS.settingsColor shared.colorScheme
            , paddingEach { left = hPad, right = 0, top = 0, bottom = 0 }
            ]

        activeItemLabel : String -> Element Msg
        activeItemLabel label =
            Components.Button.new
                { onPress = Just ResetSettingItemStatus
                , label = text label
                }
                |> Components.Button.withInline
                |> Components.Button.withLightColor
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
                    |> Components.Button.withLightColor
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
                            |> IntCrementer.withLightColor
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
                    shared.colorScheme
            ]
        , el [ height <| px 15 ] none
        , column
            settingsAttrs
            [ if model.settingsItemShown == BreathingSpeed then
                el itemAttrs <|
                    column [ width fill, spacing 20 ]
                        [ activeItemLabel "Atemgeschwindigkeit"
                        , el [ centerX ] <|
                            (Bubble.new
                                { model = model.bubble
                                , size = 70
                                }
                                |> Bubble.withLabel
                                    (shared.sessionSettings.breathCount
                                        |> Session.breathCountInt
                                        |> String.fromInt
                                    )
                                |> Bubble.view shared.colorScheme
                            )
                        , RadioGroup.new
                            { choices = Session.breathingSpeeds
                            , toString = Session.breathingSpeedDE
                            , onSelect = DefaultBreathingSpeedChanged
                            }
                            |> RadioGroup.withSelected shared.sessionSettings.breathingSpeed
                            |> RadioGroup.withLightColor True
                            |> RadioGroup.view shared.colorScheme
                        ]

              else
                viewSettingsItem
                    { label = "Atemgeschwindigkeit"
                    , value = Session.breathingSpeedDE shared.sessionSettings.breathingSpeed
                    , attributes = itemAttrs
                    , item = BreathingSpeed
                    }
                    shared.colorScheme
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
                            |> RadioGroup.withLightColor True
                            |> RadioGroup.view shared.colorScheme
                        ]

              else
                viewSettingsItem
                    { label = "Atemzüge"
                    , value = shared.sessionSettings.breathCount |> Session.breathCountInt |> String.fromInt
                    , attributes = lastItemAttrs
                    , item = BreathCount
                    }
                    shared.colorScheme
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
                            |> IntCrementer.withLightColor
                            |> IntCrementer.view shared.colorScheme shared.sessionSettings.relaxRetDuration
                        ]

              else
                viewSettingsItem
                    { label = "Erholungsretention"
                    , value = (shared.sessionSettings.relaxRetDuration |> String.fromInt) ++ " Sekunden"
                    , attributes = lastItemAttrs
                    , item = RelaxRetDuration
                    }
                    shared.colorScheme
            ]
        , let
            duration =
                Session.new shared.sessionSettings
                    |> Session.estimatedDurationMillis
                        (MotivationData.meanRetentionTimes shared.motivationData
                            |> Maybe.withDefault []
                        )
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
    -> ColorScheme
    -> Element Msg
viewSettingsItem { item, label, value, attributes } colorScheme =
    button [ width fill ]
        { onPress = Just <| SettingsItemShown item
        , label =
            row attributes
                [ text label
                , el [ alignRight, Font.color <| CS.interactInactiveDarkerColor colorScheme ] <| text value
                ]
        }


viewTechInfo : Shared.Model -> Model -> Element Msg
viewTechInfo shared model =
    el [ width fill ] <|
        el
            [ alignRight
            , Font.size 13
            , Events.onClick <| ReloadApp True
            ]
        <|
            text <|
                "Zoff Version "
                    ++ shared.currentVersion



-- el [] <|
--     case MotivationData.getMotivationData shared.motivationData of
--         Nothing ->
--             text "Noch keine Motivationsdaten vorhanden"
--         Just data ->
--             column [ spacing 5, Font.size 13 ]
--                 [ el
--                     [ Font.bold
--                     , Font.size 15
--                     , Font.color <| CS.guideColor shared.colorScheme
--                     ]
--                   <|
--                     text "Aktuell gespeicherte Motivationsdaten"
--                 , text <| "Serie: " ++ String.fromInt data.series
--                 , text <| "Letzte Sitzung: " ++ Date.toIsoString data.lastSessionDate
--                 , text <| "Mittlere Ret: " ++ (String.join "," <| List.map String.fromInt data.meanRetentiontimes)
--                 , text <| "Max Ret: " ++ String.fromInt data.maxRetention
--                 ]
