module Lib.MotivationData exposing
    ( Fields
    , MotivationData
    , encoder
    , fieldsDecoder
    , fromFields
    , lastSessionDate
    , maxRetention
    , meanRetentionTimes
    , series
    , update
    )

import Date
import Json.Decode
import Json.Encode
import Lib.Millis as Millis exposing (Milliseconds)
import Lib.SessionResults as SessionResults exposing (SessionResults)


type MotivationData
    = MotivationData Fields


type alias Fields =
    { streak : Int
    , streakFreezeDays : Float
    , lastSessionDate : Date.Date
    , meanRetentiontimes : List Milliseconds
    , maxRetention : Milliseconds
    }



-- CREATION


fromFields : Fields -> MotivationData
fromFields fields =
    MotivationData fields



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

                                --TODO: streakFreezeDays berechnen und anpassen
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

        --TODO: streakFreezeDays enkodieren
        , ( fieldnames.lastSessionDate, Json.Encode.int <| Date.toRataDie motData.lastSessionDate )
        , ( fieldnames.meanRetentiontimes
          , Json.Encode.list Json.Encode.int
                (motData.meanRetentiontimes
                    |> List.map Millis.toSeconds
                )
          )
        , ( fieldnames.maxRetention, Json.Encode.int (motData.maxRetention |> Millis.toSeconds) )
        ]


dateDecoder : Json.Decode.Decoder Date.Date
dateDecoder =
    Json.Decode.map Date.fromRataDie Json.Decode.int


fieldsDecoder : Json.Decode.Decoder Fields
fieldsDecoder =
    Json.Decode.map5
        -- TODO: Kann ich doch irgendwie direkt in MotivationData decoden?
        --       https://discourse.elm-lang.org/t/how-to-decode-json-into-a-custom-type-union-type-adt/4065/2
        Fields
        (Json.Decode.field fieldnames.series Json.Decode.int)
        --TODO: So implementieren, dass nicht das gesamte Decode fehlschlägt, wenn ein Feld fehlt
        (Json.Decode.field fieldnames.streakFreezeDays Json.Decode.float)
        (Json.Decode.field fieldnames.lastSessionDate dateDecoder)
        (Json.Decode.field fieldnames.meanRetentiontimes
            (Json.Decode.list
                (Json.Decode.int
                    |> Json.Decode.map Millis.fromSeconds
                )
            )
        )
        (Json.Decode.field fieldnames.maxRetention (Json.Decode.int |> Json.Decode.map Millis.fromSeconds))
