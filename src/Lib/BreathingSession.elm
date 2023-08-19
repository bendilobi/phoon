module Lib.BreathingSession exposing
    ( BreathingSession
    , createSession
    , currentPath
    , empty
    , goNext
    )

import Route.Path


type alias BreathingSession =
    List Route.Path.Path


empty : BreathingSession
empty =
    []


createSession : Int -> BreathingSession
createSession numberOfCycles =
    let
        cycle =
            [ Route.Path.Phases_Breathing
            , Route.Path.Phases_Retention
            , Route.Path.Phases_RelaxRetention
            ]
    in
    List.repeat numberOfCycles cycle
        |> List.concat


goNext : BreathingSession -> BreathingSession
goNext session =
    List.drop 1 session


currentPath : BreathingSession -> Route.Path.Path
currentPath session =
    case List.head session of
        Nothing ->
            Route.Path.Phases_SessionEnd

        Just path ->
            path
