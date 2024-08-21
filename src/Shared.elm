module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , appVersion, showDebugButtons
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
import Dict
import Effect exposing (Effect)
import Json.Decode
import Lib.ColorScheme as CS
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.PageFading as Fading exposing (Trigger(..))
import Lib.SafeArea as SafeArea
import Lib.Session as Session exposing (Session)
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Lib.Utils as Utils
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (UpdateState(..))
import Shared.Msg
import Task
import Time


adjustBeforeRelease =
    -- Make version string in version.json identical!!!
    ( "0.6.444", False )


appVersion =
    Tuple.first adjustBeforeRelease


showDebugButtons =
    Tuple.second adjustBeforeRelease



--- FLAGS ---


type alias Flags =
    { storedMotivationData : Json.Decode.Value
    , storedSessionSettings : Json.Decode.Value
    , storedUpdatingState : Json.Decode.Value
    , safeAreaInsets : Json.Decode.Value
    , width : Json.Decode.Value
    , height : Json.Decode.Value
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map6 Flags
        (Json.Decode.field "storedMotivationData" Json.Decode.value)
        (Json.Decode.field "storedSessionSettings" Json.Decode.value)
        (Json.Decode.field "storedUpdatingState" Json.Decode.value)
        (Json.Decode.field "safeAreaInsets" Json.Decode.value)
        (Json.Decode.field "width" Json.Decode.value)
        (Json.Decode.field "height" Json.Decode.value)



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
                    , safeAreaInsets = SafeArea.new { top = 0, bottom = 0, left = 0, right = 0 }
                    , width = 0
                    , height = 0
                    }

                Ok data ->
                    let
                        motDataDecoded =
                            Json.Decode.decodeValue MotivationData.decoder data.storedMotivationData

                        sessionSettingsDecoded =
                            Json.Decode.decodeValue Session.settingsDecoder data.storedSessionSettings

                        updateStateDecoded =
                            Json.Decode.decodeValue Json.Decode.int data.storedUpdatingState

                        safeAreasDecoded =
                            SafeArea.decode data.safeAreaInsets

                        widthDecoded =
                            Json.Decode.decodeValue Json.Decode.float data.width

                        heightDecoded =
                            Json.Decode.decodeValue Json.Decode.float data.height
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
                                    --TODO: Fehlermeldung besser formulieren...
                                    UpdateFailed "Kann Update nicht laden..."

                                else
                                    Updating <| nOfTries + 1
                    , safeAreaInsets = safeAreasDecoded
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
                    }
    in
    ( { zone = Time.utc
      , today = Date.fromRataDie 0
      , appVisible = True
      , updateState = decodedFlags.updateState
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
      , safeAreaInset = decodedFlags.safeAreaInsets
      , fadeIn = NoFade
      }
    , Effect.batch
        [ Effect.sendCmd <| Task.perform Shared.Msg.AdjustTimeZone Time.here
        , Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today
        , Effect.checkVersion Shared.Msg.ReceivedVersionOnServer
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
            , Effect.none
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
                ]
            )

        Shared.Msg.SetMotivationData motData ->
            ( { model
                | motivationData = motData
                , previousMotivationData = model.motivationData
              }
            , case motData of
                Nothing ->
                    Effect.none

                Just data ->
                    Effect.saveMotivationData data
            )

        Shared.Msg.SessionSettingsUpdated newSettings ->
            ( { model
                | sessionSettings = newSettings
                , session = Session.new newSettings
              }
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
                    if versionString == appVersion then
                        Effect.setUpdateState JustUpdated

                    else
                        Effect.updateApp model.updateState

                UpdateFailed _ ->
                    --- If the update failed for some reason, we want the user to
                    --- trigger a new updating process.
                    Effect.none

                _ ->
                    if versionString /= appVersion then
                        Effect.setUpdateState <| UpdateAvailable versionString

                    else
                        Effect.none
            )

        Shared.Msg.ReceivedVersionOnServer (Err httpError) ->
            ( { model | versionOnServer = Api.Failure httpError }
            , case model.updateState of
                Updating _ ->
                    Effect.setUpdateState <|
                        --TODO: Fehlermeldung optimieren -> ist das hier qualitativ
                        --      anders als wenn die Number of Tries überschritten wird?
                        --      httpError mit ausgeben?
                        UpdateFailed "Kann Update nicht vom Server laden"

                _ ->
                    Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.batch
        [ Browser.Events.onVisibilityChange Shared.Msg.VisibilityChanged
        , Browser.Events.onResize Shared.Msg.Resized
        , Effect.safeAreaReceiver Shared.Msg.ReceivedSafeArea
        ]
