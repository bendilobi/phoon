module Lib.MotivationData exposing
    ( MotivationData
    , decoder
    , encoder
    , lastSessionDate
    , maxRetention
    , maxStreak
    , meanRetentionTimes
    , series
    , streakFreezeDays
    , streakInfo
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
        , maxStreak : Int
        }



-- CREATION


create : Int -> Float -> Int -> Date.Date -> List Milliseconds -> Milliseconds -> Int -> MotivationData
create streak streakFreezeD streakInitTarget lastSessDate meanRetentiontimes maxRet maxStrk =
    MotivationData
        { streak = streak
        , streakFreezeDays = streakFreezeD
        , streakInitialTarget = streakInitTarget
        , lastSessionDate = lastSessDate
        , meanRetentiontimes = meanRetentiontimes
        , maxRetention = maxRet
        , maxStreak = maxStrk
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
                            , maxStreak = 1
                            }

                Just (MotivationData motData) ->
                    let
                        -- daysSinceLastSession =
                        --     Date.diff Date.Days motData.lastSessionDate today
                        { streakValid, daysSinceLastSession } =
                            streakInfo today practiceFrequencyTarget (MotivationData motData)

                        streakEndedBecauseOfTargetChange =
                            motData.streakInitialTarget > practiceFrequencyTarget

                        -- streakEnded =
                        --     (daysSinceLastSession - floor motData.streakFreezeDays)
                        --         > 1
                        --         || streakEndedBecauseOfTargetChange
                        remainingStreakFreeze =
                            if daysSinceLastSession > 0 && not streakEndedBecauseOfTargetChange then
                                -- Last session was not today so we need to apply the freeze days
                                motData.streakFreezeDays
                                    - (toFloat daysSinceLastSession - 1)
                                    |> (\freezeDays ->
                                            if streakValid then
                                                freezeDays

                                            else
                                                0
                                       )

                            else
                                motData.streakFreezeDays

                        streak =
                            if streakValid then
                                motData.streak + 1

                            else
                                -- Begin a new streak since streak freeze doesn't cover all missed days
                                1
                    in
                    Just <|
                        MotivationData
                            { motData
                                | streak = streak

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
                                    if streakValid then
                                        motData.streakInitialTarget

                                    else
                                        {- Only when we begin a new streak, we save the current practice target -}
                                        practiceFrequencyTarget
                                , lastSessionDate = today
                                , meanRetentiontimes = (mean :: motData.meanRetentiontimes) |> List.take 30
                                , maxRetention = Millis.max maxTime motData.maxRetention
                                , maxStreak = max streak motData.streak
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


maxStreak : MotivationData -> Int
maxStreak (MotivationData motData) =
    motData.maxStreak


streakFreezeDays : MotivationData -> Float
streakFreezeDays (MotivationData motData) =
    motData.streakFreezeDays


streakInitialTarget : MotivationData -> Int
streakInitialTarget (MotivationData motData) =
    motData.streakInitialTarget


streakInfo : Date.Date -> Int -> MotivationData -> { streakValid : Bool, daysSinceLastSession : Int }
streakInfo today practiceFrequencyTarget (MotivationData motData) =
    let
        daysSinceLastSession =
            Date.diff Date.Days motData.lastSessionDate today
    in
    { streakValid =
        ((daysSinceLastSession - floor motData.streakFreezeDays) < 2)
            && (motData.streakInitialTarget <= practiceFrequencyTarget)
    , daysSinceLastSession = daysSinceLastSession
    }



-- CONVERSION


fieldnames :
    { series : String
    , streakFreezeDays : String
    , streakInitialTarget : String
    , lastSessionDate : String
    , meanRetentiontimes : String
    , maxRetention : String
    , maxStreak : String
    }
fieldnames =
    { series = "series"
    , streakFreezeDays = "streakFreezeDays"
    , streakInitialTarget = "streakInitialTarget"
    , lastSessionDate = "lastSessionDate"
    , meanRetentiontimes = "meanRetentiontimes"
    , maxRetention = "maxRetention"
    , maxStreak = "maxStreak"
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
        , ( fieldnames.maxStreak, Json.Encode.int motData.maxStreak )
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
        |> optional fieldnames.maxStreak Json.Decode.int 0
