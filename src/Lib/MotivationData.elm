module Lib.MotivationData exposing
    ( MotivationData
    , decoder
    , encoder
    , lastSessionDate
    , maxRetention
    , meanRetentionTimes
    , series
    , streakFreezeDays
    , streakInitialTarget
    , update
    )

import Date
import Json.Decode exposing (float)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Lib.Millis as Millis exposing (Milliseconds)
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Round


type MotivationData
    = MotivationData
        { streak : Int
        , streakFreezeDays : Float
        , streakInitialTarget : Int
        , lastSessionDate : Date.Date
        , meanRetentiontimes : List Milliseconds
        , maxRetention : Milliseconds
        }



-- CREATION


create : Int -> Float -> Int -> Date.Date -> List Milliseconds -> Milliseconds -> MotivationData
create streak streakFreezeD streakInitTarget lastSessDate meanRetentiontimes maxRet =
    MotivationData
        { streak = streak
        , streakFreezeDays = streakFreezeD
        , streakInitialTarget = streakInitTarget
        , lastSessionDate = lastSessDate
        , meanRetentiontimes = meanRetentiontimes
        , maxRetention = maxRet
        }



-- MODIFICATION


update : SessionResults -> Date.Date -> Int -> Maybe MotivationData -> Maybe MotivationData
update results today practiceFrequencyTarget motivationData =
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

        target =
            practiceFrequencyTarget |> toFloat

        freezeIncrement =
            -- target is max 7 (as per the upper bound of the IntCrementer
            -- used in the settings). More than once per day doesn't work with
            -- the current using of one freeze per day...
            ((7 - target) / target)
                |> Round.ceilingNum 2
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
                            , streakFreezeDays = freezeIncrement
                            , streakInitialTarget = practiceFrequencyTarget
                            , lastSessionDate = today
                            , meanRetentiontimes = [ mean ]
                            , maxRetention = maxTime
                            }

                Just (MotivationData motData) ->
                    let
                        daysSinceLastSession =
                            Date.diff Date.Days motData.lastSessionDate today

                        streakEndedBecauseOfTargetChange =
                            motData.streakInitialTarget > practiceFrequencyTarget

                        streakEnded =
                            (daysSinceLastSession - floor motData.streakFreezeDays)
                                > 1
                                || streakEndedBecauseOfTargetChange

                        remainingStreakFreeze =
                            if daysSinceLastSession > 0 && not streakEndedBecauseOfTargetChange then
                                -- Last session was not today so we need to apply the freeze days
                                motData.streakFreezeDays
                                    - (toFloat daysSinceLastSession - 1)
                                    |> (\freezeDays ->
                                            if streakEnded then
                                                0

                                            else
                                                freezeDays
                                       )

                            else
                                motData.streakFreezeDays
                    in
                    Just <|
                        MotivationData
                            { motData
                                | streak =
                                    if streakEnded then
                                        -- Begin a new streak since streak freeze doesn't cover all missed days
                                        1
                                        -- else if motData.lastSessionDate == today then
                                        --     motData.streak

                                    else
                                        motData.streak + 1

                                --TODO: Faktor je nach tatsächlicher Atemzeit skalieren:
                                --      (Atemzeit * (Zuteilungsfaktor / konfigurierte Dauer einer Atemphase))
                                , streakFreezeDays =
                                    -- let
                                    --     target =
                                    --         practiceFrequencyTarget |> toFloat
                                    --     freezeIncrement =
                                    --         -- target is max 7 (as per the upper bound of the IntCrementer
                                    --         -- used in the settings). More than once per day doesn't work with
                                    --         -- the current using of one freeze per day...
                                    --         ((7 - target) / target)
                                    --             |> Round.ceilingNum 2
                                    -- in
                                    -- if remainingStreakFreeze > 8.2 then
                                    --     -- allow no more than 8 streak freezes
                                    --     8.9
                                    -- else
                                    --     remainingStreakFreeze + 0.7
                                    if remainingStreakFreeze >= 9 - freezeIncrement then
                                        -- allow no more than 8 streak freezes
                                        8.99

                                    else
                                        remainingStreakFreeze + freezeIncrement
                                , streakInitialTarget =
                                    if streakEnded then
                                        {- Only when we begin a new streak, we save the current practice target -}
                                        practiceFrequencyTarget

                                    else
                                        motData.streakInitialTarget
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


streakFreezeDays : MotivationData -> Float
streakFreezeDays (MotivationData motData) =
    motData.streakFreezeDays


streakInitialTarget : MotivationData -> Int
streakInitialTarget (MotivationData motData) =
    motData.streakInitialTarget



-- CONVERSION


fieldnames :
    { series : String
    , streakFreezeDays : String
    , streakInitialTarget : String
    , lastSessionDate : String
    , meanRetentiontimes : String
    , maxRetention : String
    }
fieldnames =
    { series = "series"
    , streakFreezeDays = "streakFreezeDays"
    , streakInitialTarget = "streakInitialTarget"
    , lastSessionDate = "lastSessionDate"
    , meanRetentiontimes = "meanRetentiontimes"
    , maxRetention = "maxRetention"
    }


encoder : MotivationData -> Json.Encode.Value
encoder (MotivationData motData) =
    Json.Encode.object
        [ ( fieldnames.series, Json.Encode.int motData.streak )
        , ( fieldnames.streakFreezeDays, Json.Encode.float motData.streakFreezeDays )
        , ( fieldnames.streakInitialTarget, Json.Encode.int motData.streakInitialTarget )
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
        |> optional fieldnames.streakInitialTarget Json.Decode.int 4
        |> required fieldnames.lastSessionDate (Json.Decode.map Date.fromRataDie Json.Decode.int)
        |> required fieldnames.meanRetentiontimes
            (Json.Decode.list
                (Json.Decode.int
                    |> Json.Decode.map Millis.fromSeconds
                )
            )
        |> required fieldnames.maxRetention (Json.Decode.int |> Json.Decode.map Millis.fromSeconds)
