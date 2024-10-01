module Pages.Information exposing (Model, Msg, page)

import Api
import Browser.Events
import Components.AnimatedButton as Button
import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Components.Dialog as Dialog
import Components.IntCrementer as IntCrementer
import Components.RadioGroup as RadioGroup
import Components.RetentionChart as RetentionChart
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Http
import Json.Decode as Decode
import Json.Encode
import Layouts
import Layouts.BaseLayout
import Layouts.BaseLayout.MainNav
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading as Fading exposing (Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Session as Session exposing (BreathCount, BreathingSpeed, Session)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model exposing (UpdateState(..))
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
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.BaseLayout_MainNav
        { header = Just "Übung optimieren"
        , headerIcon =
            Just <|
                Input.button [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ]
                    { label =
                        FeatherIcons.info
                            |> FeatherIcons.withSize 25
                            |> FeatherIcons.withStrokeWidth 1.5
                            |> FeatherIcons.toHtml []
                            |> html
                    , onPress = Just OnToggleAppInfo
                    }
        , enableScrolling = True
        , fadeOut = model.fadeOut
        , subPage =
            if shared.subPageShown then
                Just <| viewAppInfo shared model

            else
                Nothing
        , overlay = viewDialog shared model
        }



-- INIT


type alias Model =
    { settingsItemShown : SettingsItem
    , bubble : Bubble.Model Msg
    , resetSettingsButton : Button.Model
    , resetItemStatusButton : Button.Model
    , cycleCrementer : IntCrementer.Model
    , relaxRetDurCrementer : IntCrementer.Model
    , practiceFreqCrementer : IntCrementer.Model
    , dialogShown : DialogShown
    , copyButton : Button.Model
    , pasteButton : Button.Model
    , updateButton : Button.Model
    , reloadButton : Button.Model
    , replaceMotDataButton : Button.Model
    , fadeOut : Fading.Trigger
    }


type DialogShown
    = NoDialog
    | PracticeTargetWarning
    | DataPasteConfirmation MotivationData
    | DataPasteFailure Decode.Error
    | DataCopyConfirmation


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { settingsItemShown = NoItem
      , bubble =
            Bubble.init
                { bubbleType = Bubble.Counting <| Session.breathCountInt shared.sessionSettings.breathCount
                , onFinished = Nothing
                , breathingSpeed = Session.speedToMillis shared.sessionSettings.breathingSpeed
                }
      , resetSettingsButton = Button.init
      , resetItemStatusButton = Button.init
      , cycleCrementer = IntCrementer.init
      , relaxRetDurCrementer = IntCrementer.init
      , practiceFreqCrementer = IntCrementer.init
      , dialogShown = NoDialog
      , copyButton = Button.init
      , pasteButton = Button.init
      , updateButton = Button.init
      , reloadButton = Button.init
      , replaceMotDataButton = Button.init
      , fadeOut = NoFade
      }
    , if shared.versionOnServer /= Api.Loading && shared.updateState /= JustUpdated then
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
    | PracticeFrequencyTarget


type Msg
    = Tick Time.Posix
    | OnReloadButton Button.Model
    | OnUpdateButton Button.Model
    | VisibilityChanged Browser.Events.Visibility
    | DefaultCyclesChanged Int IntCrementer.Model
    | DefaultRelaxRetDurationChanged Int IntCrementer.Model
    | PracticeFreqTargetChanged Int IntCrementer.Model
    | OnDialogRestartStreak Bool
    | DefaultBreathingSpeedChanged BreathingSpeed
    | DefaultBreathCountChanged BreathCount
    | SettingsItemShown SettingsItem
    | OnResetSettingsButton Button.Model
    | OnResetItemStatusButton Button.Model
    | ReceivedNewestVersionString (Result Http.Error String)
    | OnReplaceMotivationData MotivationData
    | OnCopyButton Button.Model
    | OnPasteButton Button.Model
    | ReceivedClipboard Decode.Value
    | FadedOutBeforeUpdate
    | OnDialogCancel
    | OnToggleAppInfo


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
                    --TODO: Ausprobieren, ob ich das überhaupt brauche, oder ob das selbe
                    --      im Shared update das sowieso übernimmt
                    Effect.checkVersion ReceivedNewestVersionString
            )

        ReceivedNewestVersionString response ->
            ( model, Effect.receivedVersionOnServer response )

        OnUpdateButton newState ->
            ( { model
                | updateButton = newState
                , fadeOut =
                    if newState == Button.Triggered then
                        FadeWith <| rgb 1 1 1

                    else
                        NoFade
              }
            , if newState == Button.Triggered then
                Effect.sendCmd <| Delay.after Fading.duration FadedOutBeforeUpdate

              else
                Effect.none
            )

        FadedOutBeforeUpdate ->
            ( model
            , Effect.updateApp shared.updateState
            )

        OnReloadButton state ->
            ( { model | reloadButton = state }
            , if state == Button.Triggered then
                Effect.reload

              else
                Effect.none
            )

        DefaultCyclesChanged cycles newState ->
            let
                settings =
                    shared.sessionSettings
            in
            ( { model | cycleCrementer = newState }
            , if IntCrementer.wasTriggered newState then
                Effect.updateSessionSettings { settings | cycles = cycles }

              else
                Effect.none
            )

        DefaultRelaxRetDurationChanged seconds newState ->
            let
                settings =
                    shared.sessionSettings
            in
            ( { model | relaxRetDurCrementer = newState }
            , if IntCrementer.wasTriggered newState then
                Effect.updateSessionSettings { settings | relaxRetDuration = Millis.fromSeconds seconds }

              else
                Effect.none
            )

        PracticeFreqTargetChanged frequency newState ->
            let
                settings =
                    shared.sessionSettings

                initialFrequency =
                    shared.motivationData
                        |> Maybe.map MotivationData.streakInitialTarget
                        |> Maybe.withDefault 4
            in
            if IntCrementer.wasTriggered newState then
                ( { model
                    | practiceFreqCrementer = newState
                    , dialogShown =
                        let
                            streakValid =
                                case shared.motivationData of
                                    Nothing ->
                                        False

                                    Just motData ->
                                        MotivationData.streakInfo shared.today settings.practiceFrequencyTarget motData
                                            |> .streakValid
                        in
                        if
                            streakValid
                                && (frequency < initialFrequency)
                                {- We only want to show the dialog if the user decremented
                                   from the initialFrequency
                                -}
                                && (settings.practiceFrequencyTarget == initialFrequency)
                        then
                            PracticeTargetWarning

                        else
                            NoDialog
                  }
                , Effect.updateSessionSettings { settings | practiceFrequencyTarget = frequency }
                )

            else
                ( { model | practiceFreqCrementer = newState }, Effect.none )

        OnDialogRestartStreak endStreak ->
            let
                settings =
                    shared.sessionSettings

                initialFrequency =
                    shared.motivationData
                        |> Maybe.map MotivationData.streakInitialTarget
                        |> Maybe.withDefault 4
            in
            ( { model | dialogShown = NoDialog }
            , if endStreak then
                Effect.none

              else
                Effect.updateSessionSettings { settings | practiceFrequencyTarget = initialFrequency }
            )

        DefaultBreathingSpeedChanged speed ->
            let
                settings =
                    shared.sessionSettings
            in
            ( { model | bubble = Bubble.withSpeed (Session.speedToMillis speed) model.bubble }
            , Effect.updateSessionSettings { settings | breathingSpeed = speed }
            )

        DefaultBreathCountChanged breathCount ->
            let
                settings =
                    shared.sessionSettings
            in
            ( { model | bubble = Bubble.withBreathCount (Session.breathCountInt breathCount) model.bubble }
            , Effect.updateSessionSettings { settings | breathCount = breathCount }
            )

        OnResetSettingsButton newState ->
            ( { model | resetSettingsButton = newState }
            , if newState == Button.Triggered then
                Effect.updateSessionSettings Session.defaultSettings

              else
                Effect.none
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

        OnResetItemStatusButton newState ->
            ( { model
                | settingsItemShown =
                    if newState == Button.Triggered then
                        NoItem

                    else
                        model.settingsItemShown
                , resetItemStatusButton = newState
              }
            , Effect.none
            )

        OnReplaceMotivationData motData ->
            ( { model | dialogShown = NoDialog }
            , Effect.batch
                [ Effect.setMotivationData <| Just motData
                , Effect.toggleSubPage
                , Effect.adjustToday
                ]
            )

        OnCopyButton newState ->
            ( { model
                | copyButton = newState
                , dialogShown =
                    if newState == Button.Triggered then
                        DataCopyConfirmation

                    else
                        NoDialog
              }
            , if newState == Button.Triggered then
                case shared.motivationData of
                    Nothing ->
                        Effect.none

                    Just motData ->
                        motData
                            |> MotivationData.encoder
                            |> Json.Encode.encode 0
                            |> Effect.writeToClipboard

              else
                Effect.none
            )

        OnPasteButton newState ->
            ( { model | pasteButton = newState }
            , if newState == Button.Triggered then
                Effect.requestClipboardContent

              else
                Effect.none
            )

        ReceivedClipboard value ->
            let
                clipContent =
                    case Decode.decodeValue Decode.string value of
                        Err e ->
                            "Decode failed"

                        Ok string ->
                            string
            in
            ( { model
                | dialogShown =
                    case Decode.decodeString MotivationData.decoder clipContent of
                        Err e ->
                            DataPasteFailure e

                        Ok motDat ->
                            DataPasteConfirmation motDat
              }
            , Effect.none
            )

        OnDialogCancel ->
            ( { model | dialogShown = NoDialog }
            , Effect.none
            )

        OnToggleAppInfo ->
            ( model
            , Effect.toggleSubPage
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Sub.batch
        [ Browser.Events.onVisibilityChange VisibilityChanged
        , Effect.clipboardReceiver ReceivedClipboard
        , if model.settingsItemShown == BreathingSpeed then
            Time.every (Bubble.tickSpeed model.bubble) Tick

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
        let
            pagePadding =
                20
        in
        column
            [ width fill
            , spacing 60
            , paddingEach { left = pagePadding, right = pagePadding, top = 30, bottom = pagePadding }
            , Font.size 15
            ]
            [ viewRetentionTrend shared <| pagePadding * 2
            , viewUpdate shared model
            , viewSettings shared model
            ]
    }


viewRetentionTrend : Shared.Model -> Int -> Element msg
viewRetentionTrend shared parentPadding =
    case shared.motivationData of
        Nothing ->
            none

        Just motData ->
            let
                meanTimes =
                    MotivationData.meanRetentionTimes motData
            in
            if List.length meanTimes < 2 then
                none

            else
                column [ width fill, spacing 15 ]
                    [ el
                        [ Font.bold
                        , Font.size 20
                        , Font.color <| CS.guideColor shared.colorScheme
                        ]
                      <|
                        text "Retentionstrend"
                    , el [ centerX ] <|
                        (RetentionChart.new
                            { width = (shared.deviceInfo.window.width |> round) - (SafeArea.maxX shared.safeAreaInset * 2) - parentPadding
                            , height = 200
                            , meanRetentionTimes = meanTimes |> List.reverse |> List.map Millis.toSeconds
                            , maxRetention = MotivationData.maxRetention motData |> Millis.toSeconds
                            , meanRetentionColor = CS.guideColor shared.colorScheme
                            , maxRetentionColor = CS.seriesGoodColor shared.colorScheme
                            , copyColor = CS.interactInactiveDarkerColor shared.colorScheme
                            }
                            |> RetentionChart.view
                        )
                    , paragraph
                        [ Font.color <| CS.interactInactiveDarkerColor shared.colorScheme
                        , Font.size 13
                        , paddingXY 20 0
                        ]
                        [ text "Verlauf der gemittelten Retentionsdauern pro Sitzung (letzte 30). Die Linie oben zeigt die bisher längste Retention. " ]
                    ]


viewUpdate : Shared.Model -> Model -> Element Msg
viewUpdate shared model =
    case shared.updateState of
        UpdateAvailable versionOnServer ->
            column
                [ width fill
                , spacing 10
                , Border.rounded 10

                --TODO: Ins colorScheme aufnehmen?
                , BG.color <| rgb255 239 233 243 --243 233 236
                , padding 20
                ]
                [ paragraph [ width fill, Font.center ]
                    [ text <|
                        "Ein Update ist verfügbar von Version "
                            ++ Shared.appVersion
                            ++ " auf "
                            ++ versionOnServer
                    ]
                , Button.new
                    { model = model.updateButton
                    , label = text "Update jetzt laden"
                    , onPress = OnUpdateButton
                    }
                    |> Button.withLightColor
                    |> Button.view shared.colorScheme
                ]

        _ ->
            none


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
            Button.new
                { onPress = OnResetItemStatusButton
                , label = text label
                , model = model.resetItemStatusButton
                }
                |> Button.withInline
                |> Button.withLightColor
                |> Button.view shared.colorScheme
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
                (Button.new
                    { onPress = OnResetSettingsButton
                    , label = text "Zurücksetzen"
                    , model = model.resetSettingsButton
                    }
                    |> Button.withInline
                    |> Button.withLightColor
                    |> Button.view shared.colorScheme
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
                                        [ el
                                            [ Font.bold
                                            , Font.color <| CS.guideColor shared.colorScheme
                                            ]
                                          <|
                                            text <|
                                                String.fromInt n
                                        , text " Runde"
                                        , el [ transparent <| n == 1 ] <| text "n"
                                        ]
                            , onCrement = DefaultCyclesChanged
                            , model = model.cycleCrementer
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
        , column settingsAttrs
            [ if model.settingsItemShown == BreathingSpeed then
                el itemAttrs <|
                    column [ width fill, spacing 20 ]
                        [ activeItemLabel "Atemgeschwindigkeit"
                        , el [ centerX ] <|
                            (Bubble.new
                                { model = model.bubble
                                , size = 70
                                , bubbleColor = CS.guideColor shared.colorScheme
                                , bgColor = CS.settingsColor shared.colorScheme
                                }
                                |> Bubble.view
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
            millis =
                Session.speedToMillis shared.sessionSettings.breathingSpeed
                    |> Millis.multiplyBy (2 * Session.breathCountInt shared.sessionSettings.breathCount)
          in
          paragraph
            [ Font.size 13
            , paddingEach { bottom = 15, top = 0, left = hPad, right = 0 }
            , Font.color <| CS.interactInactiveDarkerColor shared.colorScheme
            ]
            [ text "Dauer der Atemphase: "
            , el
                [ Font.color <| CS.guideColor shared.colorScheme
                , Font.bold
                ]
              <|
                text <|
                    Millis.toString False millis
            , if Millis.toSeconds millis < 60 then
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
                                        , el
                                            [ Font.bold
                                            , Font.color <| CS.guideColor shared.colorScheme
                                            ]
                                          <|
                                            text <|
                                                String.fromInt n
                                        , text " Sekunden"
                                        ]
                            , onCrement = DefaultRelaxRetDurationChanged
                            , model = model.relaxRetDurCrementer
                            }
                            |> IntCrementer.withMin 5
                            |> IntCrementer.withMax 30
                            |> IntCrementer.withStepSize 5
                            |> IntCrementer.withLightColor
                            |> IntCrementer.view shared.colorScheme
                                (shared.sessionSettings.relaxRetDuration
                                    |> Millis.toSeconds
                                )
                        ]

              else
                viewSettingsItem
                    { label = "Erholungsretention"
                    , value =
                        (shared.sessionSettings.relaxRetDuration
                            |> Millis.toSeconds
                            |> String.fromInt
                        )
                            ++ " Sekunden"
                    , attributes = lastItemAttrs
                    , item = RelaxRetDuration
                    }
                    shared.colorScheme
            ]
        , let
            duration =
                Session.new shared.sessionSettings
                    |> Session.estimatedDurationMillis
                        (shared.motivationData
                            |> Maybe.map MotivationData.meanRetentionTimes
                            |> Maybe.withDefault []
                        )
          in
          paragraph [ paddingEach { top = 15, bottom = 0, left = 0, right = 0 } ]
            [ text "Geschätzte Gesamtdauer der Übung: "
            , el
                [ Font.color <| CS.guideColor shared.colorScheme
                , Font.bold
                ]
              <|
                text <|
                    Millis.toString False duration
            , text <|
                if Millis.toMinutes duration < 60 then
                    " Minuten"

                else
                    " Stunden"
            ]
        , el [ height <| px 15 ] none
        , column settingsAttrs
            [ if model.settingsItemShown == PracticeFrequencyTarget then
                el lastItemAttrs <|
                    column [ width fill, spacing 20 ]
                        [ activeItemLabel "Übungsziel"
                        , IntCrementer.new
                            { label =
                                \n ->
                                    paragraph []
                                        [ el
                                            [ Font.bold
                                            , Font.color <| CS.guideColor shared.colorScheme
                                            ]
                                          <|
                                            text <|
                                                String.fromInt n
                                        , text " mal pro Woche"
                                        ]
                            , onCrement = PracticeFreqTargetChanged
                            , model = model.practiceFreqCrementer
                            }
                            |> IntCrementer.withMin 1
                            -- Max 7 because more than once per day doesn't work with using one freeze
                            -- per day. If users wish to have, say, a target of two practice sessions
                            -- per day, freezes would have to be valid for half a day each...
                            |> IntCrementer.withMax 7
                            |> IntCrementer.withLightColor
                            |> IntCrementer.view shared.colorScheme
                                shared.sessionSettings.practiceFrequencyTarget
                        ]

              else
                viewSettingsItem
                    { label = "Übungsziel"
                    , value =
                        (shared.sessionSettings.practiceFrequencyTarget
                            |> String.fromInt
                        )
                            ++ " mal pro Woche"
                    , attributes = lastItemAttrs
                    , item = PracticeFrequencyTarget
                    }
                    shared.colorScheme
            ]
        , paragraph
            --TODO: Styling mit dem obigen zusammenführen ("Dauer der Atemphase"...)
            [ Font.size 13
            , paddingEach { bottom = 15, top = 0, left = hPad, right = 0 }
            , Font.color <| CS.interactInactiveDarkerColor shared.colorScheme
            ]
            [ text "Das Übungsziel bestimmt, wie häufig \"Schutzringe\" für die Fortsetzung der Serie hinzukommen. \"4 mal pro Woche\" bedeutet beispielsweise, dass für vier Übungen drei Ringe hinzukommen. Es können also drei von sieben Tagen freigenommen werden."
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
    row
        ([ Events.onClick <| SettingsItemShown item
         , pointer
         ]
            ++ attributes
        )
        [ text label
        , el [ alignRight, Font.color <| CS.interactInactiveDarkerColor colorScheme ] <| text value
        ]


viewDialog : Shared.Model -> Model -> Layouts.BaseLayout.Overlay Msg
viewDialog shared model =
    case model.dialogShown of
        NoDialog ->
            Layouts.BaseLayout.NoOverlay

        PracticeTargetWarning ->
            let
                initialFrequency =
                    shared.motivationData
                        |> Maybe.map MotivationData.streakInitialTarget
                        |> Maybe.withDefault 4
                        |> String.fromInt
            in
            Dialog.new
                { header = "Serie beenden?"
                , screenWidth = shared.deviceInfo.window.width
                , message =
                    paragraph []
                        [ text "Wenn Du das Übungsziel niedriger ansetzt als zu Beginn der Serie ("
                        , text initialFrequency
                        , text " Tage pro Woche), beginnt die Serie mit der nächsten Übung neu!"
                        ]
                , choices =
                    [ Dialog.choice
                        { label = "Serie beenden"
                        , onChoose = OnDialogRestartStreak True
                        }
                    , Dialog.choice
                        { label = "Ziel auf " ++ initialFrequency ++ " lassen"
                        , onChoose = OnDialogRestartStreak False
                        }
                    ]
                }
                |> Dialog.view shared.colorScheme
                |> Layouts.BaseLayout.ModalDialog

        DataPasteConfirmation motData ->
            Dialog.new
                { header = "Daten importieren?"
                , screenWidth = shared.deviceInfo.window.width
                , message =
                    column
                        [ width fill
                        , padding 20
                        ]
                        [ let
                            meanTimes =
                                MotivationData.meanRetentionTimes motData
                          in
                          if List.length meanTimes < 2 then
                            none

                          else
                            column [ width fill, spacing 10 ]
                                [ el [ centerX ] <| text "Retentionstrend:"
                                , column [ centerX ]
                                    [ el [ centerX ] <|
                                        (RetentionChart.new
                                            { width =
                                                (shared.deviceInfo.window.width * 0.9)
                                                    - 40
                                                    |> round

                                            -- (shared.deviceInfo.window.width |> round)
                                            --     - (SafeArea.maxX shared.safeAreaInset * 2)
                                            --     - (pagePadding + (hPad * 2))
                                            , height = 170
                                            , meanRetentionTimes = meanTimes |> List.reverse |> List.map Millis.toSeconds
                                            , maxRetention = MotivationData.maxRetention motData |> Millis.toSeconds
                                            , meanRetentionColor = CS.guideColor shared.colorScheme
                                            , maxRetentionColor = CS.seriesGoodColor shared.colorScheme
                                            , copyColor = CS.interactInactiveDarkerColor shared.colorScheme
                                            }
                                            |> RetentionChart.view
                                        )
                                    ]
                                ]
                        ]
                , choices =
                    [ Dialog.choice
                        { label = "Importieren"
                        , onChoose = OnReplaceMotivationData motData
                        }
                    , Dialog.choice
                        { label = "Verwerfen"
                        , onChoose = OnDialogCancel
                        }
                    ]
                }
                |> Dialog.view shared.colorScheme
                |> Layouts.BaseLayout.ModalDialog

        DataPasteFailure error ->
            Dialog.new
                { header = "Einfügen nicht möglich"
                , screenWidth = shared.deviceInfo.window.width
                , message = paragraph [] [ text "Das scheinen leider keine validen Ergebnisdaten zu sein..." ]
                , choices =
                    [ Dialog.choice
                        { label = "Schließen"
                        , onChoose = OnDialogCancel
                        }
                    ]
                }
                |> Dialog.view shared.colorScheme
                |> Layouts.BaseLayout.ModalDialog

        DataCopyConfirmation ->
            Dialog.new
                { header = "Kopieren erfolgreich"
                , screenWidth = shared.deviceInfo.window.width
                , message = paragraph [] [ text "Deine Übungsergebnisse wurden in die Zwischenablage kopiert und stehen zum Einfügen in anderen Apps zur Verfügung!" ]
                , choices =
                    [ Dialog.choice
                        { label = "Schließen"
                        , onChoose = OnDialogCancel
                        }
                    ]
                }
                |> Dialog.view shared.colorScheme
                |> Layouts.BaseLayout.ModalDialog


viewAppInfo :
    Shared.Model
    -> Model
    -> Layouts.BaseLayout.MainNav.SubPage Msg
viewAppInfo shared model =
    { header = "App Info"
    , content =
        column
            ([ width fill
             , height fill
             , Font.size 15
             , spacing 50
             , padding 30
             ]
                ++ CS.primaryInformation shared.colorScheme
            )
            [ column
                [ width fill
                , spacing 10
                , Font.color <| CS.guideColor shared.colorScheme
                ]
                [ el
                    [ centerX

                    {- Without the explicit width and height here, Safari doesn't show the
                       image if clip is applied
                    -}
                    , width <| px 100
                    , height <| px 100
                    , Border.rounded 27
                    , clip
                    ]
                  <|
                    image [ width <| px 100 ]
                        { src = "/img/logo/favicon.png"
                        , description = "Zoff App Logo"
                        }
                , el [ height <| px 0 ] none
                , el
                    [ Font.size 30
                    , centerX
                    , Font.extraBold
                    ]
                  <|
                    text
                        "Zoff"
                , el [ centerX, Font.size 17, Font.semiBold ] <| text "Wim Hof Atmung mit dem Hauch von Zen"
                , el [ centerX, Font.size 14 ] <| text <| "Version " ++ Shared.appVersion
                ]
            , column [ spacing 15 ]
                [ paragraph [] [ text "Diese App wurde mit Hingebung für Dich programmiert von Benno Dielmann." ]
                , paragraph []
                    [ text "Hast Du Fragen, Verbesserungsvorschläge, Ideen, Kritik? Schreibe "
                    , link []
                        { url = "mailto:mail@benno-dielmann.de?subject=Zoff Feedback"
                        , label =
                            el
                                [ Font.underline
                                , Font.color <| CS.interactActiveLighterColor shared.colorScheme
                                ]
                            <|
                                text "mir eine E-Mail"
                        }
                    , text "!"
                    ]
                ]
            , case shared.motivationData of
                Nothing ->
                    none

                _ ->
                    column
                        [ width fill
                        , spacing 20
                        , padding 15
                        , Border.rounded 10
                        , BG.color <| rgb 1 1 1
                        ]
                        [ text "Datenmanagement"
                        , Button.new
                            { onPress = OnCopyButton
                            , label = text "Übungsergebnisse kopieren"
                            , model = model.copyButton
                            }
                            |> Button.withLightColor
                            |> Button.view shared.colorScheme
                        , Button.new
                            { model = model.pasteButton
                            , label = text "Übungsergebnisse importieren"
                            , onPress = OnPasteButton
                            }
                            |> Button.withLightColor
                            |> Button.view shared.colorScheme
                        , Button.new
                            { onPress = OnReloadButton
                            , label = text "App neu laden"
                            , model = model.reloadButton
                            }
                            |> Button.withLightColor
                            |> Button.view shared.colorScheme
                        ]
            , column [ Font.size 11, spacing 10 ]
                [ el [ Font.bold ] <| text "Technische Informationen:"
                , text <|
                    "Browser-Sprache: "
                        ++ (case shared.appLanguage of
                                Shared.Model.En ->
                                    "Englisch"

                                Shared.Model.De ->
                                    "Deutsch"
                           )
                , text <|
                    "Standalone: "
                        ++ (case shared.standalone of
                                Nothing ->
                                    "Kein Wert"

                                Just s ->
                                    if s then
                                        "Ja"

                                    else
                                        "Nein"
                           )
                ]
            ]
    }
