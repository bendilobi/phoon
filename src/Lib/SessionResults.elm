module Lib.SessionResults exposing
    ( SessionResults
    , addRetention
    , currentRetentionTime
    , empty
    , finishedCycles
    , getRetentionTimes
    , incrementCurrentRetention
    )

-- TODO: Sollte ich das umbauen, sodass es besser mit currentCycle im BreathingSession harmoniert?
--       Oder dort integrieren und dann dafür sorgen, dass BreathingSession.new auch gleich die
--       Results leert?


type SessionResults
    = NoResults
    | Results (List Int) Int


empty : SessionResults
empty =
    NoResults


addRetention : SessionResults -> SessionResults
addRetention results =
    case results of
        NoResults ->
            Results [] 0

        Results list current ->
            Results (list ++ [ current ]) 0


incrementCurrentRetention : SessionResults -> SessionResults
incrementCurrentRetention results =
    case results of
        NoResults ->
            Results [] 1

        Results list current ->
            Results list (current + 1)


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


finishedCycles : SessionResults -> Int
finishedCycles results =
    getRetentionTimes results
        |> List.length
