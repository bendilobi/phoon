module Pages.Information exposing (Model, Msg, page)

-- import Components.Button as UButton

import Api
import Browser.Events
import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Components.IntCrementer as IntCrementer
import Components.RadioGroup as RadioGroup
import Components.RetentionChart as RetentionChart
import Components.StatelessAnimatedButton as Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode
import Json.Encode
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.SafeArea as SafeArea
import Lib.Session as Session exposing (BreathCount, BreathingSpeed, Session)
import Page exposing (Page)
import Route exposing (Route)
import Shared
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
    Layouts.MainNav
        { header = Just "Übung optimieren"
        , enableScrolling = True
        }



-- INIT


type ClipboardData value
    = NoData
    | Success value
    | Failure Json.Decode.Error


type alias Model =
    { settingsItemShown : SettingsItem
    , bubble : Bubble.Model Msg
    , resetSettingsButton : Button.Model
    , resetItemStatusButton : Button.Model
    , cycleCrementer : IntCrementer.Model
    , relaxRetDurCrementer : IntCrementer.Model
    , copyButton : Button.Model
    , pasteButton : Button.Model
    , updateButton : Button.Model
    , reloadButton : Button.Model
    , pastedMotivationData : ClipboardData MotivationData
    , replaceMotDataButton : Button.Model
    , testButton : Button.Model

    {- If the user starts to scroll while having a button pressed, the button never gets a "pointerup"
       event and thus stays in "Pressed" state. As a workaround, we only animate buttons if the page
       is not scrolling. And since Safari doesn't support the "scrollend" event, we have to set
       scrolling to True again whenever the user presses a button
    -}
    -- , scrolling : Bool
    }


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
      , copyButton = Button.init
      , pasteButton = Button.init
      , updateButton = Button.init
      , reloadButton = Button.init
      , pastedMotivationData = NoData
      , replaceMotDataButton = Button.init
      , testButton = Button.init

      --   , scrolling = False
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
    | AppControls


type Msg
    = Tick Time.Posix
    | OnReloadButton Button.Model
    | OnUpdateButton Button.Model
    | VisibilityChanged Browser.Events.Visibility
    | DefaultCyclesChanged Int IntCrementer.Model
    | DefaultRelaxRetDurationChanged Int IntCrementer.Model
    | DefaultBreathingSpeedChanged BreathingSpeed
    | DefaultBreathCountChanged BreathCount
    | SettingsItemShown SettingsItem
    | OnResetSettingsButton Button.Model
    | OnResetItemStatusButton Button.Model
    | ReceivedNewestVersionString (Result Http.Error String)
    | OnReplaceMotivationDataButton MotivationData Button.Model
    | OnCopyButton Button.Model
    | OnPasteButton Button.Model
    | ReceivedClipboard Json.Decode.Value
      -- | OnScroll
    | OnTestButton Button.Model


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

        OnUpdateButton state ->
            ( { model | updateButton = state }
              --, scrolling = False }
            , if state == Button.Released then
                Effect.updateApp

              else
                Effect.none
            )

        OnReloadButton state ->
            ( { model | reloadButton = state }
              --, scrolling = False }
            , if state == Button.Released then
                Effect.reload

              else
                Effect.none
            )

        DefaultCyclesChanged cycles state ->
            let
                settings =
                    shared.sessionSettings
            in
            ( { model | cycleCrementer = state }
            , Effect.updateSessionSettings { settings | cycles = cycles }
            )

        DefaultRelaxRetDurationChanged seconds state ->
            let
                settings =
                    shared.sessionSettings
            in
            ( { model | relaxRetDurCrementer = state }
            , Effect.updateSessionSettings { settings | relaxRetDuration = Millis.fromSeconds seconds }
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
              --, scrolling = False }
            , if newState == Button.Released then
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
                    if newState == Button.Released then
                        NoItem

                    else
                        model.settingsItemShown
                , resetItemStatusButton = newState

                -- , scrolling = False
              }
            , Effect.none
            )

        OnReplaceMotivationDataButton motData newState ->
            ( { model | replaceMotDataButton = newState }
              --, scrolling = False }
            , if newState == Button.Released then
                Effect.setMotivationData motData

              else
                Effect.none
            )

        OnCopyButton state ->
            --TODO: Ausprobieren, ob ich einfach der gesamten Seite einen onscroll-Handler geben kann
            --      statt jedem einzelnen Button...
            ( { model | copyButton = state }
              --, scrolling = False }
            , case state of
                Button.Released ->
                    case shared.motivationData of
                        Nothing ->
                            Effect.none

                        Just motData ->
                            motData
                                |> MotivationData.encoder
                                |> Json.Encode.encode 0
                                |> Effect.writeToClipboard

                _ ->
                    Effect.none
            )

        OnPasteButton state ->
            ( { model | pasteButton = state }
              --, scrolling = False }
            , if state == Button.Released then
                Effect.requestClipboardContent

              else
                Effect.none
            )

        ReceivedClipboard value ->
            let
                clipContent =
                    case Json.Decode.decodeValue Json.Decode.string value of
                        Err e ->
                            "Decode failed"

                        Ok string ->
                            string

                pastedMotivationData =
                    case Json.Decode.decodeString MotivationData.fieldsDecoder clipContent of
                        Err e ->
                            Failure e

                        Ok fields ->
                            Success <| MotivationData.fromFields fields
            in
            ( { model | pastedMotivationData = pastedMotivationData }
            , Effect.none
            )

        -- OnScroll ->
        --     --TODO: Ich muss hier doch die Models der Buttons zurücksetzen, oder?
        --     ( model, Effect.none )
        OnTestButton newState ->
            ( { model | testButton = newState }, Effect.none )



-- ( { model | scrolling = True }, Effect.none )
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
            [ viewIntroduction shared model
            , viewUpdate shared model
            , viewRetentionTrend shared <| pagePadding * 2
            , viewSettings shared model <| pagePadding * 2
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

        -- , Button.new
        --     { onPress = OnTestButton
        --     , model = model.testButton
        --     , label = text "blah"
        --     }
        --     |> Button.view shared.colorScheme
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
                    [ paragraph [ width fill, Font.center ]
                        [ text <|
                            "Ein Update ist verfügbar von Version "
                                ++ shared.currentVersion
                                ++ " auf "
                                ++ versionOnServer
                        ]
                    , Button.new
                        { model = model.updateButton
                        , label = text "Update jetzt laden"
                        , onPress = OnUpdateButton
                        }
                        -- |> Button.withLightColor
                        -- |> Button.withAnimated (not model.scrolling)
                        |> Button.view shared.colorScheme
                    ]

            else
                none


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
                    ]


viewSettings : Shared.Model -> Model -> Int -> Element Msg
viewSettings shared model pagePadding =
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
                -- |> Button.withAnimated (not model.scrolling)
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
                    -- |> Button.withAnimated (not model.scrolling)
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
                                        [ el [ Font.bold ] <| text <| String.fromInt n
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
            ]
            [ text "Dauer der Atemphase: "
            , el
                [ Font.color <| CS.guideColor shared.colorScheme
                , Font.bold
                ]
              <|
                text <|
                    Millis.toString millis
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
                                        , el [ Font.bold ] <| text <| String.fromInt n
                                        , text " Sekunden"
                                        ]
                            , onCrement = DefaultRelaxRetDurationChanged
                            , model = model.relaxRetDurCrementer
                            }
                            |> IntCrementer.withMin 5
                            |> IntCrementer.withMax 30
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

            -- |> Millis.toSeconds
          in
          paragraph [ paddingEach { top = 15, bottom = 0, left = 0, right = 0 } ]
            [ text "Geschätzte Gesamtdauer der Übung: "
            , el
                [ Font.color <| CS.guideColor shared.colorScheme
                , Font.bold
                ]
              <|
                text <|
                    Millis.toString duration
            , text <|
                if Millis.toMinutes duration < 60 then
                    " Minuten"

                else
                    " Stunden"
            ]
        , if model.settingsItemShown == AppControls then
            --TODO: Was, wenn noch keine Motivationsdaten vorhanden?
            --      Buttons deaktivieren oder gar nicht anzeigen?
            el [ width fill, paddingEach { top = 50, bottom = 0, left = 0, right = 0 } ] <|
                column settingsAttrs
                    [ el itemAttrs <|
                        column [ width fill, spacing 20 ]
                            [ activeItemLabel "Spezialwerkzeuge..."
                            , Button.new
                                { onPress = OnCopyButton
                                , label = text "Übungsergebnisse kopieren"
                                , model = model.copyButton
                                }
                                -- |> Button.withLightColor
                                -- |> Button.withAnimated (not model.scrolling)
                                |> Button.view shared.colorScheme
                            , Button.new
                                { model = model.pasteButton
                                , label = text "Übungsergebnisse einfügen"
                                , onPress = OnPasteButton
                                }
                                -- |> Button.withLightColor
                                -- |> Button.withAnimated (not model.scrolling)
                                |> Button.view shared.colorScheme
                            , case model.pastedMotivationData of
                                NoData ->
                                    none

                                Failure err ->
                                    paragraph [ Font.color <| CS.actionNeededColor shared.colorScheme ]
                                        [ text "Das scheinen leider keine validen Ergebnisdaten zu sein..."
                                        ]

                                Success motData ->
                                    column [ width fill, spacing 20 ]
                                        [ el
                                            [ Font.color <| CS.successColor shared.colorScheme
                                            , Font.bold
                                            ]
                                          <|
                                            text "Daten erfolgreich importiert!"

                                        --TODO: Was sonst noch über die Daten zeigen?
                                        , let
                                            meanTimes =
                                                MotivationData.meanRetentionTimes motData
                                          in
                                          if List.length meanTimes < 2 then
                                            none

                                          else
                                            column [ width fill, spacing 10 ]
                                                [ text "Retentionstrend:"
                                                , column [ centerX ]
                                                    [ RetentionChart.new
                                                        { width =
                                                            (shared.deviceInfo.window.width |> round)
                                                                - (SafeArea.maxX shared.safeAreaInset * 2)
                                                                - (pagePadding + (hPad * 2))
                                                        , height = 200
                                                        , meanRetentionTimes = meanTimes |> List.reverse |> List.map Millis.toSeconds
                                                        , maxRetention = MotivationData.maxRetention motData |> Millis.toSeconds
                                                        , meanRetentionColor = CS.guideColor shared.colorScheme
                                                        , maxRetentionColor = CS.seriesGoodColor shared.colorScheme
                                                        , copyColor = CS.interactInactiveDarkerColor shared.colorScheme
                                                        }
                                                        |> RetentionChart.view
                                                    , el
                                                        [ Font.size 15
                                                        , centerX
                                                        , paddingXY 0 50
                                                        ]
                                                      <|
                                                        (Button.new
                                                            { onPress = OnReplaceMotivationDataButton motData
                                                            , label = text "Eingefügte Ergebnisse übernehmen"
                                                            , model = model.replaceMotDataButton
                                                            }
                                                            |> Button.withLightColor
                                                            |> Button.withInline
                                                            -- |> Button.withAnimated (not model.scrolling)
                                                            |> Button.view shared.colorScheme
                                                        )
                                                    ]
                                                ]
                                        ]
                            ]
                    , el lastItemAttrs <|
                        (Button.new
                            { onPress = OnReloadButton
                            , label = text "App neu laden"
                            , model = model.reloadButton
                            }
                            -- |> Button.withLightColor
                            -- |> Button.withAnimated (not model.scrolling)
                            |> Button.view shared.colorScheme
                        )
                    ]

          else
            none
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
    Input.button [ width fill ]
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
            , Events.onClick <| SettingsItemShown AppControls
            ]
        <|
            text <|
                "Zoff Version "
                    ++ shared.currentVersion
