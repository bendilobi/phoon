module Lib.MotivationData exposing (MotivationData, empty, getMotivationData, update)

import Date
import Lib.SessionResults as SessionResults exposing (SessionResults)


type MotivationData
    = NoData
    | MotivationData
        { series : Int
        , lastSession : Date.Date
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
            SessionResults.getRetentionTimes results |> List.maximum
    in
    case motivationData of
        NoData ->
            case meanRetTime of
                Nothing ->
                    NoData

                Just mean ->
                    MotivationData
                        { series = 1
                        , lastSession = today
                        , meanRetentiontimes = [ mean ]
                        , maxRetention = maxRetTime |> Maybe.withDefault 0
                        }

        MotivationData motData ->
            case meanRetTime of
                Nothing ->
                    MotivationData motData

                Just mean ->
                    MotivationData
                        { motData
                          -- TODO: Nur inkrementieren, wenn today - 1 Tag != yesterday, ansonsten series = 1
                            | series = motData.series + 1
                            , lastSession = today
                            , meanRetentiontimes = (mean :: motData.meanRetentiontimes) |> List.take 30

                            -- TODO: bisheriges Maximum nehmen
                            , maxRetention = maxRetTime |> Maybe.withDefault motData.maxRetention
                        }


getMotivationData :
    MotivationData
    ->
        Maybe
            { series : Int
            , lastSession : Date.Date
            , meanRetentiontimes : List Int
            , maxRetention : Int
            }
getMotivationData motData =
    case motData of
        NoData ->
            Nothing

        MotivationData data ->
            Just data
