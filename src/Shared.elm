module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
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
import Lib.SafeArea as SafeArea
import Lib.Session as Session exposing (Session)
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Lib.Utils as Utils
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Time


version =
    "0.6.171"



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
                    , isUpdating = False
                    , safeAreaInsets = SafeArea.new { top = 0, bottom = 0, left = 0, right = 0 }
                    , width = 0
                    , height = 0
                    }

                Ok data ->
                    let
                        motDataDecoded =
                            Json.Decode.decodeValue MotivationData.fieldsDecoder data.storedMotivationData

                        sessionSettingsDecoded =
                            Json.Decode.decodeValue Session.settingsDecoder data.storedSessionSettings

                        isUpdatingDecoded =
                            Json.Decode.decodeValue Json.Decode.bool data.storedUpdatingState

                        safeAreasDecoded =
                            SafeArea.decode data.safeAreaInsets

                        widthDecoded =
                            Json.Decode.decodeValue Json.Decode.int data.width

                        heightDecoded =
                            Json.Decode.decodeValue Json.Decode.int data.height
                    in
                    { motData =
                        case motDataDecoded of
                            Err e ->
                                Nothing

                            Ok fields ->
                                Just <| MotivationData.fromFields fields
                    , sessionSettings =
                        case sessionSettingsDecoded of
                            Err e ->
                                Session.defaultSettings

                            Ok settings ->
                                settings
                    , isUpdating =
                        case isUpdatingDecoded of
                            Err e ->
                                False

                            Ok updating ->
                                updating
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
      , currentVersion = version
      , appVisible = True
      , versionOnServer = Api.Loading
      , deviceInfo = Utils.classifyDevice { width = decodedFlags.width, height = decodedFlags.height }
      , session = Session.new decodedFlags.sessionSettings
      , results = SessionResults.empty
      , previousPath = Route.Path.Home_
      , motivationData = decodedFlags.motData
      , colorScheme = CS.newSunrise
      , sessionSettings = decodedFlags.sessionSettings
      , appIsUpdating = decodedFlags.isUpdating
      , justUpdated = False
      , baseApiUrl = "/"
      , safeAreaInset = decodedFlags.safeAreaInsets
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
            ( { model | deviceInfo = Utils.classifyDevice { width = round viewport.width, height = round viewport.height } }
            , Effect.none
            )

        Shared.Msg.Resized width height ->
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
                        | justUpdated = False
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

        Shared.Msg.ResultsUpdated results ->
            ( { model | results = results }
            , Effect.none
            )

        Shared.Msg.NavigateTriggered path ->
            ( { model
                | previousPath = route.path
                , results =
                    if route.path == Session.phasePath Session.RelaxRetention then
                        SessionResults.addRetention model.results

                    else
                        model.results
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
                    Session.Cancelled ->
                        Effect.none

                    Session.Finished ->
                        MotivationData.update model.results model.today model.motivationData
                            |> Shared.Msg.SetMotivationData
                            |> Effect.sendMsg
                , Effect.navigate Route.Path.Home_
                ]
            )

        Shared.Msg.SessionSettingsUpdated newSettings ->
            ( { model
                | sessionSettings = newSettings
                , session = Session.new newSettings
              }
            , Effect.saveSessionSettings newSettings
            )

        Shared.Msg.SetUpdating updating ->
            ( { model
                | appIsUpdating = updating
                , justUpdated = model.appIsUpdating && not updating
              }
            , Effect.saveUpdatingState updating
            )

        Shared.Msg.ReceivedVersionOnServer (Ok versionString) ->
            ( { model
                | versionOnServer = Api.Success versionString
              }
            , if versionString == model.currentVersion then
                Effect.setUpdating False

              else if model.appIsUpdating then
                Effect.reload

              else
                Effect.none
            )

        Shared.Msg.ReceivedVersionOnServer (Err httpError) ->
            ( { model | versionOnServer = Api.Failure httpError }
            , Effect.setUpdating False
            )

        Shared.Msg.SetMotivationData motData ->
            ( { model | motivationData = motData }
            , case motData of
                Nothing ->
                    Effect.none

                Just data ->
                    Effect.saveMotivationData data
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.batch
        [ Browser.Events.onVisibilityChange Shared.Msg.VisibilityChanged
        , Browser.Events.onResize Shared.Msg.Resized
        , Effect.safeAreaReceiver Shared.Msg.ReceivedSafeArea
        ]
