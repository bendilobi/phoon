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

-- TODO: Sollte ich das umbauen, sodass es besser mit currentCycle im Session harmoniert?
--       Oder dort integrieren und dann dafür sorgen, dass Session.new auch gleich die
--       Results leert?


type SessionResults
    = NoResults
    | StartedCounting Int
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
            StartedCounting 0

        StartedCounting n ->
            Results [ n ] 0

        Results list current ->
            Results (list ++ [ current ]) 0


incrementCurrentRetention : SessionResults -> SessionResults
incrementCurrentRetention results =
    case results of
        NoResults ->
            StartedCounting 1

        StartedCounting n ->
            StartedCounting (n + 1)

        Results list current ->
            Results list (current + 1)



-- INTROSPECTION


currentRetentionTime : SessionResults -> Int
currentRetentionTime results =
    case results of
        NoResults ->
            0

        StartedCounting n ->
            n

        Results _ current ->
            current


getRetentionTimes : SessionResults -> Maybe (List Int)
getRetentionTimes results =
    case results of
        Results list _ ->
            Just list

        _ ->
            Nothing


meanRetentionTime : SessionResults -> Maybe Int
meanRetentionTime results =
    case results of
        Results retTimes _ ->
            let
                n =
                    List.length retTimes
                        |> toFloat
            in
            if n == 0 then
                Nothing

            else
                retTimes
                    |> List.sum
                    |> toFloat
                    |> (\sum -> sum / n)
                    |> round
                    |> Just

        _ ->
            Nothing


finishedCycles : SessionResults -> Int
finishedCycles results =
    getRetentionTimes results
        |> Maybe.map List.length
        |> Maybe.withDefault 0
