module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , sessionHintsID, subPageClosingTime
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Api
import Browser.Dom
import Browser.Events
import Date
import Delay
import Dict
import Effect exposing (Effect)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Lib.ColorScheme as CS
import Lib.MotivationData as MotivationData
import Lib.PageFading as Fading exposing (Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Texts as Texts
import Lib.Utils as Utils
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (UpdateState(..))
import Shared.Msg
import Task
import Time
import Version


subPageClosingTime =
    500


sessionHintsID : String
sessionHintsID =
    "sessionHints"



--- FLAGS ---


type alias Flags =
    { storedMotivationData : Json.Decode.Value
    , storedSessionSettings : Json.Decode.Value
    , storedUpdatingState : Json.Decode.Value
    , storedShowWakelockNote : Json.Decode.Value
    , width : Json.Decode.Value
    , height : Json.Decode.Value
    , browserLang : Json.Decode.Value
    , standalone : Json.Decode.Value
    , iOSVersion : Json.Decode.Value
    }


makeFlags :
    Json.Decode.Value
    -> Json.Decode.Value
    -> Json.Decode.Value
    -> Json.Decode.Value
    -> Json.Decode.Value
    -> Json.Decode.Value
    -> Json.Decode.Value
    -> Json.Decode.Value
    -> Json.Decode.Value
    -> Flags
makeFlags mot set upd wlh wid hei bro stan ios =
    { storedMotivationData = mot
    , storedSessionSettings = set
    , storedUpdatingState = upd
    , storedShowWakelockNote = wlh
    , width = wid
    , height = hei
    , browserLang = bro
    , standalone = stan
    , iOSVersion = ios
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed makeFlags
        |> required "storedMotivationData" Json.Decode.value
        |> required "storedSessionSettings" Json.Decode.value
        |> required "storedUpdatingState" Json.Decode.value
        |> required "storedShowWakelockNote" Json.Decode.value
        |> required "width" Json.Decode.value
        |> required "height" Json.Decode.value
        |> required "browserLang" Json.Decode.value
        |> required "standalone" Json.Decode.value
        |> required "iOSVersion" Json.Decode.value



--- INIT ---


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        decodedFlags =
            case flagsResult of
                Err e ->
                    { motData = Nothing
                    , sessionSettings = Session.defaultSettings
                    , updateState = NotUpdating
                    , showWakelockNote = True
                    , width = 0
                    , height = 0
                    , browserLang = Texts.En
                    , standalone = Nothing
                    , iOSVersion = Nothing
                    }

                Ok data ->
                    let
                        motDataDecoded =
                            Json.Decode.decodeValue MotivationData.decoder data.storedMotivationData

                        sessionSettingsDecoded =
                            Json.Decode.decodeValue Session.settingsDecoder data.storedSessionSettings

                        updateStateDecoded =
                            Json.Decode.decodeValue Json.Decode.int data.storedUpdatingState

                        showWakelockNoteDecoded =
                            Json.Decode.decodeValue Json.Decode.bool data.storedShowWakelockNote

                        widthDecoded =
                            Json.Decode.decodeValue Json.Decode.float data.width

                        heightDecoded =
                            Json.Decode.decodeValue Json.Decode.float data.height

                        browserLangDecoded =
                            Json.Decode.decodeValue (Json.Decode.string |> Json.Decode.andThen Texts.browserLanguageDecoder) data.browserLang

                        browserLang =
                            case browserLangDecoded of
                                Err _ ->
                                    Texts.En

                                Ok lang ->
                                    lang

                        standaloneDecoded =
                            Json.Decode.decodeValue Json.Decode.bool data.standalone

                        iOSVersionDecoded =
                            Json.Decode.decodeValue Json.Decode.string data.iOSVersion
                    in
                    { motData =
                        case motDataDecoded of
                            Err e ->
                                Nothing

                            Ok motDat ->
                                Just motDat
                    , sessionSettings =
                        case sessionSettingsDecoded of
                            Err e ->
                                Session.defaultSettings

                            Ok settings ->
                                settings
                    , updateState =
                        case updateStateDecoded of
                            Err e ->
                                NotUpdating

                            Ok nOfTries ->
                                if nOfTries < 0 then
                                    NotUpdating

                                else if nOfTries == 10 then
                                    UpdateFailed <| Texts.updateFailedNOfTries browserLang

                                else
                                    Updating <| nOfTries + 1
                    , showWakelockNote =
                        case showWakelockNoteDecoded of
                            Err e ->
                                True

                            Ok hint ->
                                hint
                    , width =
                        case widthDecoded of
                            Err e ->
                                0

                            Ok px ->
                                px
                    , height =
                        case heightDecoded of
                            Err e ->
                                0

                            Ok px ->
                                px
                    , browserLang = browserLang
                    , standalone =
                        case standaloneDecoded of
                            {- Supposedly, this value (navigator.standalone) is a Bool in iOS and "undefined" in other OSes
                               In iOS it is "True" if the app is called via a link from the home screen and "False" if not.
                            -}
                            Err e ->
                                Nothing

                            Ok s ->
                                Just s
                    , iOSVersion =
                        case iOSVersionDecoded of
                            Err e ->
                                Nothing

                            Ok v ->
                                String.toInt v
                    }
    in
    ( { zone = Time.utc
      , today = Date.fromRataDie 0
      , appLanguage = decodedFlags.browserLang
      , iOSVersion = decodedFlags.iOSVersion
      , pointerIsMouse = Nothing
      , appVisible = True
      , updateState = decodedFlags.updateState
      , showWakelockNote = decodedFlags.showWakelockNote
      , versionOnServer = Api.Loading
      , deviceInfo = Utils.classifyDevice { width = decodedFlags.width, height = decodedFlags.height }
      , session = Session.new decodedFlags.sessionSettings
      , results = SessionResults.empty
      , previousPath = Route.Path.Home_
      , motivationData = decodedFlags.motData
      , previousMotivationData = Nothing
      , colorScheme = CS.newSunrise
      , sessionSettings = decodedFlags.sessionSettings
      , baseApiUrl = "/version/"
      , safeAreaInset = SafeArea.blanco
      , fadeIn = NoFade
      , infoWindowState = Shared.Model.Closed
      , sessionHintsHeight = Nothing
      , subPageShown = False
      , subPageClosingInProgress = False
      , standalone = decodedFlags.standalone
      }
    , Effect.batch
        [ Effect.sendCmd <| Task.perform Shared.Msg.AdjustTimeZone Time.here
        , Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today
        , Effect.checkVersion Shared.Msg.ReceivedVersionOnServer

        {- Getting the safe area insets via flag at startup doesn't seem to work (they are always 0),
           so we do this here:
           (maybe safe areas are availably only after some time:
           https://stackoverflow.com/questions/64891541/get-safe-area-inset-bottom-on-page-load)
        -}
        , Effect.sendCmd <| Delay.after 1000 <| Shared.Msg.RequestSafeArea
        ]
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.ReceivedViewport { viewport } ->
            ( { model | deviceInfo = Utils.classifyDevice { width = viewport.width, height = viewport.height } }
            , Effect.getSessionHintsHeight
            )

        Shared.Msg.Resized _ _ ->
            --- There seems to be a timing issue: "sometimes" the width or height received
            --- here after a change between portrait and landscape mode is wrong. Therefore,
            --- we get the viewport size in an additional step:
            ( model
            , Effect.batch
                [ Effect.getSafeArea
                , Effect.sendCmd <| Task.perform Shared.Msg.ReceivedViewport Browser.Dom.getViewport
                ]
            )

        Shared.Msg.RequestSafeArea ->
            ( model
            , Effect.getSafeArea
            )

        Shared.Msg.ReceivedSafeArea value ->
            ( { model
                | safeAreaInset = SafeArea.decode value
              }
            , Effect.none
            )

        Shared.Msg.VisibilityChanged visibility ->
            case visibility of
                Browser.Events.Hidden ->
                    ( { model
                        | updateState =
                            case model.updateState of
                                JustUpdated ->
                                    NotUpdating

                                _ ->
                                    model.updateState
                        , appVisible = False
                      }
                    , Effect.none
                    )

                Browser.Events.Visible ->
                    ( { model | appVisible = True }
                    , Effect.batch
                        --- In case the next day came or the user changed time zones while
                        --- the app was suspended...
                        [ Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today
                        , Effect.sendCmd <| Task.perform Shared.Msg.AdjustTimeZone Time.here
                        ]
                    )

        Shared.Msg.AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Effect.none
            )

        Shared.Msg.GetToday ->
            ( model, Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today )

        Shared.Msg.AdjustToday today ->
            let
                practicedToday =
                    case model.motivationData of
                        Nothing ->
                            False

                        Just motData ->
                            MotivationData.lastSessionDate motData == today
            in
            ( { model
                | today = today
                , colorScheme =
                    if practicedToday then
                        CS.newDaylight

                    else
                        CS.newSunrise
              }
            , Effect.none
            )

        Shared.Msg.PointerDevice isMouse ->
            ( { model | pointerIsMouse = Just <| isMouse }, Effect.none )

        Shared.Msg.SessionUpdated session ->
            ( { model | session = session }
            , Effect.none
            )

        Shared.Msg.CancelSession session ->
            let
                sessionAtEnd =
                    Session.jumpToEnd model.session
            in
            ( { model | session = sessionAtEnd }
            , Effect.navigate NoFade <| Session.currentPath sessionAtEnd
            )

        Shared.Msg.ResultsUpdated results ->
            ( { model | results = results }
            , Effect.none
            )

        Shared.Msg.NavigateTriggered fade path ->
            ( { model
                | previousPath = route.path
                , fadeIn = fade
                , subPageShown = False
              }
            , Effect.replaceRoute
                { path = path
                , query = Dict.empty
                , hash = Nothing
                }
            )

        Shared.Msg.SessionEnded endType ->
            ( { model
                | session = Session.new model.sessionSettings
              }
            , Effect.batch
                [ case endType of
                    Session.Discarded ->
                        Effect.sendMsg <| Shared.Msg.SetMotivationData model.previousMotivationData

                    Session.Finished ->
                        Effect.none
                , Effect.navigate (FadeWith Fading.sessionFadingColor) Route.Path.Home_
                , Effect.sendMsg <| Shared.Msg.SetInfoWindowState Shared.Model.Half
                , Effect.releaseWakeLock
                ]
            )

        Shared.Msg.SetMotivationData motData ->
            ( { model
                | motivationData = motData
                , previousMotivationData = model.motivationData
              }
            , Effect.saveMotivationData motData
            )

        Shared.Msg.SessionSettingsUpdated newSettings ->
            ( { model | sessionSettings = newSettings }
            , Effect.saveSessionSettings newSettings
            )

        Shared.Msg.SetUpdateState newState ->
            ( { model | updateState = newState }
            , Effect.saveUpdatingState newState
            )

        Shared.Msg.ReceivedVersionOnServer (Ok versionString) ->
            ( { model
                | versionOnServer = Api.Success versionString
              }
            , case model.updateState of
                Updating _ ->
                    if versionString == Version.appVersion then
                        Effect.batch
                            [ Effect.setUpdateState JustUpdated
                            , Effect.toggleSubPage
                            ]

                    else
                        Effect.updateApp model.updateState

                UpdateFailed _ ->
                    --- If the update failed for some reason, we want the user to
                    --- trigger a new updating process.
                    Effect.none

                _ ->
                    if versionString /= Version.appVersion then
                        Effect.setUpdateState <| UpdateAvailable versionString

                    else
                        Effect.none
            )

        Shared.Msg.ReceivedVersionOnServer (Err httpError) ->
            ( { model | versionOnServer = Api.Failure httpError }
            , case model.updateState of
                Updating _ ->
                    Effect.setUpdateState <|
                        UpdateFailed <|
                            Texts.updateFailedServer model.appLanguage

                _ ->
                    Effect.none
            )

        Shared.Msg.SetInfoWindowState state ->
            ( { model | infoWindowState = state }
            , Effect.none
            )

        Shared.Msg.SessionHintsHeightRequested ->
            ( model
            , Effect.sendCmd <| Task.attempt Shared.Msg.ReceivedSessionHintsElement <| Browser.Dom.getElement sessionHintsID
            )

        Shared.Msg.ReceivedSessionHintsElement (Ok { element }) ->
            ( { model
                | sessionHintsHeight = Just element.height
              }
            , Effect.none
            )

        Shared.Msg.ReceivedSessionHintsElement (Err _) ->
            ( { model | sessionHintsHeight = Nothing }
            , Effect.none
            )

        Shared.Msg.PrepareToggleSubPage ->
            ( { model
                | subPageClosingInProgress = model.subPageShown

                {- If it's already shown, we don't change that because we do that after the delay
                   for the Transition animation. If it's closed, we immediately show it:
                -}
                , subPageShown = True
              }
            , if model.subPageShown then
                Effect.sendCmd <| Delay.after subPageClosingTime Shared.Msg.OnToggleSubPage

              else
                Effect.none
            )

        Shared.Msg.OnToggleSubPage ->
            ( { model
                | subPageShown = not model.subPageShown
                , subPageClosingInProgress = False
              }
            , Effect.none
            )

        Shared.Msg.OnToggleShowWakelockNote ->
            let
                hintShown =
                    not model.showWakelockNote
            in
            ( { model | showWakelockNote = hintShown }
            , Effect.saveShowWakelockNote hintShown
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.batch
        [ Browser.Events.onVisibilityChange Shared.Msg.VisibilityChanged
        , Browser.Events.onResize Shared.Msg.Resized
        , Effect.safeAreaReceiver Shared.Msg.ReceivedSafeArea
        ]
