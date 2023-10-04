module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    -- , navigateNext
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Browser.Events
import Date
import Dict
import Effect exposing (Effect)
import Json.Decode
import Lib.ColorScheme as CS
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.Session as Session exposing (Session)
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Time



-- FLAGS


type alias Flags =
    { storedMotivationData : Json.Decode.Value
    , storedSessionSettings : Json.Decode.Value
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map2 Flags
        -- (Json.Decode.field "storedMotivationData" MotivationData.fieldsDecoder)
        (Json.Decode.field "storedMotivationData" Json.Decode.value)
        -- (Json.Decode.field "storedSessionSettings" Session.settingsDecoder)
        (Json.Decode.field "storedSessionSettings" Json.Decode.value)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        ( motData, sessionSettings ) =
            --TODO: Konzept überlegen, wie mit Fehlern hier umgegangen werden soll
            case flagsResult of
                Err e ->
                    -- let
                    --     blah =
                    --         Debug.log "error" e
                    -- in
                    ( MotivationData.empty
                    , Session.defaultSettings
                    )

                Ok data ->
                    let
                        motDataDecoded =
                            Json.Decode.decodeValue MotivationData.fieldsDecoder data.storedMotivationData

                        sessionSettingsDecoded =
                            Json.Decode.decodeValue Session.settingsDecoder data.storedSessionSettings
                    in
                    ( case motDataDecoded of
                        Err e ->
                            MotivationData.empty

                        Ok fields ->
                            MotivationData.fromFields fields
                    , case sessionSettingsDecoded of
                        Err e ->
                            Session.defaultSettings

                        Ok settings ->
                            settings
                    )
    in
    ( { zone = Time.utc
      , today = Date.fromRataDie 0
      , session = Session.new sessionSettings
      , results = SessionResults.empty
      , previousPath = Route.Path.Home_
      , motivationData = motData
      , colorScheme = CS.newSunrise
      , sessionSettings = sessionSettings
      }
    , Effect.batch
        [ Effect.sendCmd <| Task.perform Shared.Msg.AdjustTimeZone Time.here
        , Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today
        ]
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.VisibilityChanged visibility ->
            ( model
            , case visibility of
                Browser.Events.Hidden ->
                    Effect.none

                Browser.Events.Visible ->
                    Effect.sendCmd <| Task.perform Shared.Msg.AdjustToday Date.today
            )

        Shared.Msg.AdjustTimeZone newZone ->
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

        Shared.Msg.SessionEnded ->
            let
                newMotData =
                    MotivationData.update model.results model.today model.motivationData
            in
            ( { model
                | session = Session.new model.sessionSettings
                , motivationData = newMotData
              }
            , Effect.batch
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



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Browser.Events.onVisibilityChange Shared.Msg.VisibilityChanged
