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
    { message : String }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.field "message" Json.Decode.string
        |> Json.Decode.map Flags



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        storedData =
            case flagsResult of
                Err _ ->
                    "Fehler..."

                Ok data ->
                    data.message
    in
    ( { zone = Time.utc
      , session = Session.new
      , results = SessionResults.empty
      , previousPath = Route.Path.Home_
      , storedData = storedData
      , motivationData = MotivationData.empty
      }
    , Effect.sendCmd <| Task.perform Shared.Msg.AdjustTimeZone Time.here
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

        -- TODO: Im Elm Land Discord fragen, wie man mit den Effekten ein Task.andThen macht
        Shared.Msg.SessionEndedX ->
            ( model, Effect.sendCmd <| Task.perform Shared.Msg.SessionEnded Date.today )

        Shared.Msg.SessionEnded today ->
            -- TODO: Motivationsdaten speichern veranlassen
            ( { model
                | session = Session.new
                , motivationData = MotivationData.update model.results today model.motivationData
              }
            , Effect.navigate Route.Path.Home_
            )


navigateNext : Session -> Effect msg
navigateNext session =
    case Session.goNext session of
        Just sess ->
            Effect.batch
                [ Effect.sessionUpdated sess
                , Effect.navigate <| Session.phasePath <| Session.currentPhase sess
                ]

        Nothing ->
            -- TODO: Ich würde gerne eine SessionEnded-Message mit dem aktuellen
            --       Datum schicken -> wie geht das?
            -- Date.today
            --     |> Task.andThen Effect.sessionEnded
            --     |> Effect.sendCmd
            Effect.sessionEnded



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
