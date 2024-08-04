module Lib.MotivationData exposing
    ( MotivationData
    , decoder
    , encoder
    , lastSessionDate
    , maxRetention
    , meanRetentionTimes
    , series
    , update
    )

import Date
import Json.Decode exposing (float)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Lib.Millis as Millis exposing (Milliseconds)
import Lib.SessionResults as SessionResults exposing (SessionResults)


type MotivationData
    = MotivationData
        { streak : Int
        , streakFreezeDays : Float
        , lastSessionDate : Date.Date
        , meanRetentiontimes : List Milliseconds
        , maxRetention : Milliseconds
        }



-- CREATION


create : Int -> Float -> Date.Date -> List Milliseconds -> Milliseconds -> MotivationData
create streak streakFreezeDays lastSessDate meanRetentiontimes maxRet =
    MotivationData
        { streak = streak
        , streakFreezeDays = streakFreezeDays
        , lastSessionDate = lastSessDate
        , meanRetentiontimes = meanRetentiontimes
        , maxRetention = maxRet
        }



-- MODIFICATION


update : SessionResults -> Date.Date -> Maybe MotivationData -> Maybe MotivationData
update results today motivationData =
    let
        meanRetTime : Maybe Milliseconds
        meanRetTime =
            SessionResults.meanRetentionTime results
                |> Maybe.map Millis.fromSeconds

        maxRetTime : Milliseconds
        maxRetTime =
            SessionResults.getRetentionTimes results
                |> Maybe.andThen List.maximum
                |> Maybe.withDefault 0
                |> Millis.fromSeconds
    in
    case meanRetTime of
        Nothing ->
            --- No results to add
            motivationData

        Just meanTime ->
            let
                mean =
                    meanTime

                maxTime =
                    maxRetTime
            in
            case motivationData of
                Nothing ->
                    Just <|
                        MotivationData
                            { streak = 1
                            , streakFreezeDays = 0
                            , lastSessionDate = today
                            , meanRetentiontimes = [ mean ]
                            , maxRetention = maxTime
                            }

                -- )
                Just (MotivationData motData) ->
                    Just <|
                        MotivationData
                            { motData
                                | streak =
                                    if Date.diff Date.Days today motData.lastSessionDate < -1 then
                                        -- Last session is longer ago than yesterday, so we start at 1
                                        1

                                    else if motData.lastSessionDate == today then
                                        --TODO: Trotzdem hochzählen, d.h. Streak ist Übungen, nicht Tage
                                        motData.streak

                                    else
                                        motData.streak + 1

                                --TODO: Faktor konfigurierbar machen
                                --TODO: Faktor je nach tatsächlicher Atemzeit skalieren:
                                --      (Atemzeit * (Zuteilungsfaktor / konfigurierte Dauer einer Atemphase))
                                , streakFreezeDays = motData.streakFreezeDays + 0.7
                                , lastSessionDate = today
                                , meanRetentiontimes = (mean :: motData.meanRetentiontimes) |> List.take 30
                                , maxRetention = Millis.max maxTime motData.maxRetention
                            }



-- ACCESS


series : MotivationData -> Int
series (MotivationData motData) =
    motData.streak


lastSessionDate : MotivationData -> Date.Date
lastSessionDate (MotivationData motData) =
    motData.lastSessionDate


meanRetentionTimes : MotivationData -> List Milliseconds
meanRetentionTimes (MotivationData motData) =
    motData.meanRetentiontimes


maxRetention : MotivationData -> Milliseconds
maxRetention (MotivationData motData) =
    motData.maxRetention



-- CONVERSION


fieldnames :
    { series : String
    , streakFreezeDays : String
    , lastSessionDate : String
    , meanRetentiontimes : String
    , maxRetention : String
    }
fieldnames =
    { series = "series"
    , streakFreezeDays = "streakFreezeDays"
    , lastSessionDate = "lastSessionDate"
    , meanRetentiontimes = "meanRetentiontimes"
    , maxRetention = "maxRetention"
    }


encoder : MotivationData -> Json.Encode.Value
encoder (MotivationData motData) =
    Json.Encode.object
        [ ( fieldnames.series, Json.Encode.int motData.streak )
        , ( fieldnames.streakFreezeDays, Json.Encode.float motData.streakFreezeDays )
        , ( fieldnames.lastSessionDate, Json.Encode.int <| Date.toRataDie motData.lastSessionDate )
        , ( fieldnames.meanRetentiontimes
          , Json.Encode.list Json.Encode.int
                (motData.meanRetentiontimes
                    |> List.map Millis.toSeconds
                )
          )
        , ( fieldnames.maxRetention, Json.Encode.int (motData.maxRetention |> Millis.toSeconds) )
        ]


decoder : Json.Decode.Decoder MotivationData
decoder =
    Json.Decode.succeed create
        |> optional fieldnames.series Json.Decode.int 0
        |> optional fieldnames.streakFreezeDays Json.Decode.float 0
        |> required fieldnames.lastSessionDate (Json.Decode.map Date.fromRataDie Json.Decode.int)
        |> required fieldnames.meanRetentiontimes
            (Json.Decode.list
                (Json.Decode.int
                    |> Json.Decode.map Millis.fromSeconds
                )
            )
        |> required fieldnames.maxRetention (Json.Decode.int |> Json.Decode.map Millis.fromSeconds)
