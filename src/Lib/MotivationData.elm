module Lib.MotivationData exposing (Fields, MotivationData, empty, encoder, fieldsDecoder, fromFields, getMotivationData, lastSessionDate, series, update)

import Date
import Json.Decode
import Json.Encode
import Lib.SessionResults as SessionResults exposing (SessionResults)


type
    MotivationData
    -- TODO: Ist das zu umständlich mit NoData vs MotivationData?
    --       Alternative wäre: Den Fall, dass es keine Daten gibt, außerhalb
    --       handhaben. => Ist wohl besser. MotivationData ist eben Daten, nicht NoData...
    = NoData
    | MotivationData Fields


type alias Fields =
    { series : Int
    , lastSessionDate : Date.Date
    , meanRetentiontimes : List Int
    , maxRetention : Int
    }



-- CREATION


empty : MotivationData
empty =
    NoData


fromFields : Fields -> MotivationData
fromFields fields =
    MotivationData fields



-- MODIFICATION


update : SessionResults -> Date.Date -> MotivationData -> MotivationData
update results today motivationData =
    let
        meanRetTime =
            SessionResults.meanRetentionTime results

        maxRetTime =
            SessionResults.getRetentionTimes results
                |> List.maximum
                |> Maybe.withDefault 0
    in
    case motivationData of
        NoData ->
            case meanRetTime of
                Nothing ->
                    NoData

                Just mean ->
                    -- If there are results, but no preexisting motivation data, initialize the latter
                    MotivationData
                        { series = 1
                        , lastSessionDate = today
                        , meanRetentiontimes = [ mean ]
                        , maxRetention = maxRetTime
                        }

        MotivationData motData ->
            case meanRetTime of
                -- There is preexisting motivation data, but no results
                Nothing ->
                    motivationData

                Just mean ->
                    -- There is preexisting motivation data and there are results
                    MotivationData
                        { motData
                            | series =
                                if Date.diff Date.Days today motData.lastSessionDate < -1 then
                                    -- Last session is longer ago than yesterday, so we start at 1
                                    1

                                else if motData.lastSessionDate == today then
                                    motData.series

                                else
                                    motData.series + 1
                            , lastSessionDate = today
                            , meanRetentiontimes = (mean :: motData.meanRetentiontimes) |> List.take 30
                            , maxRetention = max maxRetTime motData.maxRetention
                        }



-- ACCESS


getMotivationData : MotivationData -> Maybe Fields
getMotivationData motData =
    -- TODO: benutze ich gerade zum debuggen. Wenns nicht mehr benötigt ist, löschen und durch einzelne
    --       access funktionen ersetzen
    case motData of
        NoData ->
            Nothing

        MotivationData data ->
            Just data


series : MotivationData -> Maybe Int
series motData =
    case motData of
        NoData ->
            Nothing

        MotivationData data ->
            Just <| data.series


lastSessionDate : MotivationData -> Maybe Date.Date
lastSessionDate motData =
    case motData of
        NoData ->
            Nothing

        MotivationData data ->
            Just <| data.lastSessionDate



-- CONVERSION


fieldnames :
    { series : String
    , lastSessionDate : String
    , meanRetentiontimes : String
    , maxRetention : String
    }
fieldnames =
    { series = "series"
    , lastSessionDate = "lastSessionDate"
    , meanRetentiontimes = "meanRetentiontimes"
    , maxRetention = "maxRetention"
    }


encoder : MotivationData -> Json.Encode.Value
encoder motData =
    case motData of
        NoData ->
            Json.Encode.null

        MotivationData data ->
            Json.Encode.object
                [ ( fieldnames.series, Json.Encode.int data.series )
                , ( fieldnames.lastSessionDate, Json.Encode.int <| Date.toRataDie data.lastSessionDate )
                , ( fieldnames.meanRetentiontimes, Json.Encode.list Json.Encode.int data.meanRetentiontimes )
                , ( fieldnames.maxRetention, Json.Encode.int data.maxRetention )
                ]


dateDecoder : Json.Decode.Decoder Date.Date
dateDecoder =
    Json.Decode.map Date.fromRataDie Json.Decode.int


fieldsDecoder : Json.Decode.Decoder Fields
fieldsDecoder =
    Json.Decode.map4
        -- TODO: Kann ich doch irgendwie direkt in MotivationData decoden?
        --       https://discourse.elm-lang.org/t/how-to-decode-json-into-a-custom-type-union-type-adt/4065/2
        Fields
        (Json.Decode.field fieldnames.series Json.Decode.int)
        (Json.Decode.field fieldnames.lastSessionDate dateDecoder)
        (Json.Decode.field fieldnames.meanRetentiontimes (Json.Decode.list Json.Decode.int))
        (Json.Decode.field fieldnames.maxRetention Json.Decode.int)
