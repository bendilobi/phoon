module Lib.BreathingSession exposing
    ( BreathingSession
    , createSession
    , currentCycle
    , currentPath
    , empty
    , goNext
    )

import Route.Path


type BreathingSession
    = BreathingSession
        { phases : List Route.Path.Path
        , currentCycle : Int
        }


empty : BreathingSession
empty =
    BreathingSession
        { phases = []
        , currentCycle = 0
        }


createSession : Int -> BreathingSession
createSession numberOfCycles =
    let
        -- TODO: Auch Start und Ende - Phasen in phases aufnehmen
        cycle =
            [ Route.Path.Phases_Breathing
            , Route.Path.Phases_Retention
            , Route.Path.Phases_RelaxRetention
            ]
    in
    BreathingSession
        { phases =
            Route.Path.Phases_SessionStart
                :: (List.repeat numberOfCycles cycle
                        |> List.concat
                   )
        , currentCycle = 0
        }


goNext : BreathingSession -> BreathingSession
goNext (BreathingSession session) =
    let
        phases =
            List.drop 1 session.phases
    in
    BreathingSession
        { phases = phases
        , currentCycle =
            if List.head phases == Just Route.Path.Phases_Breathing then
                session.currentCycle + 1

            else
                session.currentCycle
        }


currentPath : BreathingSession -> Route.Path.Path
currentPath (BreathingSession session) =
    case List.head session.phases of
        Nothing ->
            Route.Path.Phases_SessionEnd

        Just path ->
            path


currentCycle : BreathingSession -> Int
currentCycle (BreathingSession session) =
    session.currentCycle
