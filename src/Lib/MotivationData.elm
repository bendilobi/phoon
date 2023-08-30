module Lib.MotivationData exposing (MotivationData, empty, getMotivationData, update)

import Date
import Lib.SessionResults as SessionResults exposing (SessionResults)


type MotivationData
    = NoData
    | MotivationData
        { series : Int
        , lastSessionDate : Date.Date
        , meanRetentiontimes : List Int
        , maxRetention : Int
        }


empty : MotivationData
empty =
    NoData


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
                    MotivationData motData

                Just mean ->
                    -- There is preexisting motivation data and there are results
                    MotivationData
                        { motData
                            | series =
                                if Date.diff Date.Days today motData.lastSessionDate < -1 then
                                    -- Last session is longer ago than yesterday, so we start at 1
                                    1

                                else
                                    -- TODO: soll auch inkrementiert werden, wenn mehrere Sessions an einem Tag?
                                    motData.series + 1
                            , lastSessionDate = today
                            , meanRetentiontimes = (mean :: motData.meanRetentiontimes) |> List.take 30
                            , maxRetention = max maxRetTime motData.maxRetention
                        }


getMotivationData :
    MotivationData
    ->
        Maybe
            { series : Int
            , lastSessionDate : Date.Date
            , meanRetentiontimes : List Int
            , maxRetention : Int
            }
getMotivationData motData =
    case motData of
        NoData ->
            Nothing

        MotivationData data ->
            Just data
