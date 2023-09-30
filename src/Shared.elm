module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , navigateNext
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

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
    { storedMotivationData : MotivationData.Fields
    , storedSessionSettings : Session.Settings
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map2 Flags
        (Json.Decode.field "storedMotivationData" MotivationData.fieldsDecoder)
        (Json.Decode.field "storedSessionSettings" Session.settingsDecoder)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        ( motData, sessionSettings ) =
            case flagsResult of
                Err e ->
                    --TODO: Styling und Methode für Fehlermeldungen implementieren und
                    --      hier eine Meldung zeigen
                    --TODO: Wie können die Fehler in einzelnen Flags erkannt und behandelt werden?
                    ( MotivationData.empty
                    , { cycles = 4
                      , relaxRetDuration = 15
                      , breathingSpeed = Session.Medium
                      , breathCount = Session.Forty
                      }
                    )

                Ok data ->
                    ( MotivationData.fromFields data.storedMotivationData
                    , data.storedSessionSettings
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


navigateNext : Session -> Effect msg
navigateNext session =
    case Session.goNext session of
        Just sess ->
            Effect.batch
                [ --TODO: kann ich hier in Shared auch direkt machen...
                  Effect.sessionUpdated sess
                , Effect.navigate <| Session.phasePath <| Session.currentPhase sess
                ]

        Nothing ->
            Effect.sessionEnded



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
