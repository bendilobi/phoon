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
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Time


version =
    "0.6.96"



--- FLAGS ---


type alias Flags =
    { storedMotivationData : Json.Decode.Value
    , storedSessionSettings : Json.Decode.Value
    , storedUpdatingState : Json.Decode.Value
    , safeAreaInsetLeft : String
    , sar : String
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map5 Flags
        (Json.Decode.field "storedMotivationData" Json.Decode.value)
        (Json.Decode.field "storedSessionSettings" Json.Decode.value)
        (Json.Decode.field "storedUpdatingState" Json.Decode.value)
        (Json.Decode.field "safeAreaInsetLeft" Json.Decode.string)
        (Json.Decode.field "sar" Json.Decode.string)



--- INIT ---


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        decodedFlags =
            --TODO: Konzept überlegen, wie mit Fehlern hier umgegangen werden soll
            case flagsResult of
                Err e ->
                    { motData = MotivationData.empty
                    , sessionSettings = Session.defaultSettings
                    , isUpdating = False
                    , sal = 0
                    , sar = 0
                    }

                Ok data ->
                    let
                        motDataDecoded =
                            Json.Decode.decodeValue MotivationData.fieldsDecoder data.storedMotivationData

                        sessionSettingsDecoded =
                            Json.Decode.decodeValue Session.settingsDecoder data.storedSessionSettings

                        isUpdatingDecoded =
                            Json.Decode.decodeValue Json.Decode.bool data.storedUpdatingState

                        salDecoded =
                            data.safeAreaInsetLeft
                                |> extractSafeAreaSize

                        sarDecoded =
                            data.sar |> extractSafeAreaSize
                    in
                    { motData =
                        case motDataDecoded of
                            Err e ->
                                MotivationData.empty

                            Ok fields ->
                                MotivationData.fromFields fields
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
                    , sal =
                        case salDecoded of
                            Nothing ->
                                0

                            Just px ->
                                px
                    , sar =
                        case sarDecoded of
                            Nothing ->
                                0

                            Just px ->
                                px
                    }
    in
    ( { zone = Time.utc
      , today = Date.fromRataDie 0
      , currentVersion = version
      , versionOnServer = Api.Loading
      , windowSize = { width = 0, height = 0 }
      , session = Session.new decodedFlags.sessionSettings
      , results = SessionResults.empty
      , previousPath = Route.Path.Home_
      , motivationData = decodedFlags.motData
      , colorScheme = CS.newSunrise
      , sessionSettings = decodedFlags.sessionSettings
      , appIsUpdating = decodedFlags.isUpdating
      , justUpdated = False
      , baseApiUrl = "/"
      , safeAreaInset = SafeArea.new { left = decodedFlags.sal, right = decodedFlags.sar, top = 0, bottom = 0 }
      }
    , Effect.batch
        [ Effect.sendCmd <| Task.perform Shared.Msg.AdjustTimeZone Time.here
        , Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today
        , Effect.sendCmd <| Task.perform Shared.Msg.ReceivedViewport Browser.Dom.getViewport
        , Effect.checkVersion Shared.Msg.ReceivedVersionOnServer
        ]
    )


extractSafeAreaSize : String -> Maybe Int
extractSafeAreaSize string =
    string
        |> String.split "px"
        |> List.head
        |> Maybe.andThen String.toInt
        --- It seems Apple adds 15 pixels to the actual size of the notch (iPhone XR...)
        |> Maybe.map (\sal -> sal - 15)
        |> Maybe.map (max 0)



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.ReceivedViewport { viewport } ->
            ( { model | windowSize = { width = round viewport.width, height = round viewport.height } }
            , Effect.none
            )

        Shared.Msg.Resized width height ->
            --- There seems to be a timing issue: "sometimes" the width or height received
            --- here after a change between portrait and landscape mode is wrong. Therefore,
            --- we get the viewport size in an additional step:
            ( model
              --{ model | windowSize = { width = width, height = height } }
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
            --TODO: Prüfen -> Brauche ich das eigentlich während der Sitzung?
            --      Oder sollte ich das mit Today vielleicht ins MainNav bewegen?
            case visibility of
                Browser.Events.Hidden ->
                    ( { model | justUpdated = False }, Effect.none )

                Browser.Events.Visible ->
                    ( model, Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today )

        Shared.Msg.AdjustTimeZone newZone ->
            --- TODO: Das auch immer machen, wenn App visible wird?
            ( { model | zone = newZone }
            , Effect.none
            )

        Shared.Msg.AdjustToday today ->
            let
                practicedToday =
                    case MotivationData.lastSessionDate model.motivationData of
                        Nothing ->
                            False

                        Just date ->
                            date == today
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

        Shared.Msg.SessionEnded cancelled ->
            let
                newMotData =
                    if cancelled then
                        model.motivationData

                    else
                        MotivationData.update model.results model.today model.motivationData
            in
            ( { model
                | session = Session.new model.sessionSettings
                , motivationData = newMotData
              }
            , Effect.batch
                --Todo: Effect.sendMsg Shared.Msg.SetMotivationData
                [ Effect.saveMotivationData newMotData
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
            , Effect.saveMotivationData motData
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.batch
        [ Browser.Events.onVisibilityChange Shared.Msg.VisibilityChanged
        , Browser.Events.onResize Shared.Msg.Resized
        , Effect.safeAreaReceiver Shared.Msg.ReceivedSafeArea
        ]
