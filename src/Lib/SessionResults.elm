module Lib.SessionResults exposing
    ( SessionResults
    , addRetention
    , currentRetentionTime
    , empty
    , finishedCycles
    , getRetentionTimes
    , incrementCurrentRetention
    , meanRetentionTime
    )

-- TODO: Sollte ich das umbauen, sodass es besser mit currentCycle im BreathingSession harmoniert?
--       Oder dort integrieren und dann dafür sorgen, dass BreathingSession.new auch gleich die
--       Results leert?


type SessionResults
    = NoResults
    | Results (List Int) Int



-- CREATION


empty : SessionResults
empty =
    NoResults



-- UPDATE


addRetention : SessionResults -> SessionResults
addRetention results =
    case results of
        NoResults ->
            -- We add a retention that hasn't been incremented...
            Results [ 0 ] 0

        Results list current ->
            Results (list ++ [ current ]) 0


incrementCurrentRetention : SessionResults -> SessionResults
incrementCurrentRetention results =
    case results of
        NoResults ->
            Results [] 1

        Results list current ->
            Results list (current + 1)



-- INTROSPECTION


currentRetentionTime : SessionResults -> Int
currentRetentionTime results =
    case results of
        NoResults ->
            0

        Results _ current ->
            current


getRetentionTimes : SessionResults -> List Int
getRetentionTimes results =
    case results of
        NoResults ->
            []

        Results list _ ->
            list


meanRetentionTime : SessionResults -> Maybe Int
meanRetentionTime results =
    case results of
        NoResults ->
            Nothing

        Results retTimes currentRetTime ->
            retTimes
                |> List.sum
                |> toFloat
                |> (\sum -> sum / (toFloat <| List.length retTimes))
                |> round
                |> Just


finishedCycles : SessionResults -> Int
finishedCycles results =
    getRetentionTimes results
        |> List.length
