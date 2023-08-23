module Lib.BreathingSession exposing
    ( BreathingSession
    , addCycle
    , createSession
    , currentCycle
    , currentPath
    , empty
    , goNext
    , jumpToEnd
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


createPhases : Int -> List Route.Path.Path
createPhases numberOfCycles =
    let
        cycle =
            [ Route.Path.Phases_Breathing
            , Route.Path.Phases_Retention
            , Route.Path.Phases_RelaxRetention
            ]
    in
    Route.Path.Phases_SessionStart
        :: (List.repeat numberOfCycles cycle
                |> List.concat
           )
        ++ [ Route.Path.Phases_SessionEnd ]


createSession : Int -> BreathingSession
createSession numberOfCycles =
    BreathingSession
        { phases = createPhases numberOfCycles
        , currentCycle = 0
        }


addCycle : BreathingSession -> BreathingSession
addCycle (BreathingSession session) =
    BreathingSession
        { phases = createPhases 1
        , currentCycle = session.currentCycle
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


jumpToEnd : BreathingSession -> BreathingSession
jumpToEnd (BreathingSession session) =
    BreathingSession
        { phases = List.drop (List.length session.phases - 1) session.phases
        , currentCycle = session.currentCycle
        }


currentPath : BreathingSession -> Route.Path.Path
currentPath (BreathingSession session) =
    case List.head session.phases of
        Nothing ->
            Route.Path.Home_

        Just path ->
            path


currentCycle : BreathingSession -> Int
currentCycle (BreathingSession session) =
    session.currentCycle
