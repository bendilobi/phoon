module Lib.BreathingSession exposing
    ( BreathingSession
    , createSession
    , currentPath
    , goNext
    )

import Route exposing (Route)
import Route.Path


type alias BreathingSession =
    List Route.Path.Path


createSession : BreathingSession
createSession =
    [ Route.Path.Phases_Breathing
    , Route.Path.Phases_Retention
    , Route.Path.Phases_RelaxRetention
    , Route.Path.Phases_Breathing
    , Route.Path.Phases_Retention
    , Route.Path.Phases_RelaxRetention
    ]


goNext : BreathingSession -> BreathingSession
goNext session =
    List.drop 1 session


currentPath : BreathingSession -> Route.Path.Path
currentPath session =
    case List.head session of
        Nothing ->
            Route.Path.SessionEnd

        Just path ->
            path
