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
    { storedMotivationData : MotivationData.Fields }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.field "storedMotivationData" MotivationData.fieldsDecoder
        |> Json.Decode.map Flags



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        motData =
            case flagsResult of
                Err e ->
                    --TODO: Styling und Methode für Fehlermeldungen implementieren und
                    --      hier eine Meldung zeigen
                    MotivationData.empty

                Ok data ->
                    MotivationData.fromFields data.storedMotivationData
    in
    ( { zone = Time.utc
      , today = Date.fromRataDie 0

      --TODO: Settings von localStorage kommen lassen
      , session = Session.new { cycles = 4, relaxRetDuration = 15 }
      , results = SessionResults.empty
      , previousPath = Route.Path.Home_
      , motivationData = motData
      , colorScheme = CS.newSunrise
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
                --TODO: Settings verwenden
                | session = Session.new { cycles = 4, relaxRetDuration = 15 }
                , motivationData = newMotData
              }
            , Effect.batch
                [ Effect.saveMotivationData newMotData
                , Effect.navigate Route.Path.Home_
                ]
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
