module Lib.BreathingSession exposing
    ( BreathingSession
    , addCycle
    , breathCount
    , currentCycle
    , currentPath
    , empty
    , goNext
    , jumpToEnd
    , new
    , relaxRetDuration
    , speedMillis
      -- , estimatedDuration
    , withFiftyBreaths
    , withRelaxRetDuration
    , withSpeedQuick
    , withThirtyBreaths
    )

import Route.Path


type BreathingSpeed
    = Slow
    | Normal
    | Quick


type BreathCount
    = Thirty
    | Forty
    | Fifty


type BreathingSession
    = BreathingSession
        { phases : List Route.Path.Path
        , currentCycle : Int
        , breathCount : BreathCount
        , breathingSpeed : BreathingSpeed
        , relaxRetentionDuration : Int
        }


new : { cycles : Int } -> BreathingSession
new props =
    BreathingSession
        { phases = createPhases props.cycles
        , currentCycle = 0
        , breathCount = Forty
        , breathingSpeed = Normal
        , relaxRetentionDuration = 15
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


empty : BreathingSession
empty =
    new { cycles = 0 }


withThirtyBreaths : BreathingSession -> BreathingSession
withThirtyBreaths (BreathingSession session) =
    BreathingSession { session | breathCount = Thirty }


withFiftyBreaths : BreathingSession -> BreathingSession
withFiftyBreaths (BreathingSession session) =
    BreathingSession { session | breathCount = Fifty }


withSpeedSlow : BreathingSession -> BreathingSession
withSpeedSlow (BreathingSession session) =
    BreathingSession { session | breathingSpeed = Slow }


withSpeedQuick : BreathingSession -> BreathingSession
withSpeedQuick (BreathingSession session) =
    BreathingSession { session | breathingSpeed = Quick }


withRelaxRetDuration : Int -> BreathingSession -> BreathingSession
withRelaxRetDuration dur (BreathingSession session) =
    BreathingSession { session | relaxRetentionDuration = dur }


addCycle : BreathingSession -> BreathingSession
addCycle (BreathingSession session) =
    BreathingSession
        { session
            | phases = createPhases 1
            , currentCycle = session.currentCycle
        }


goNext : BreathingSession -> BreathingSession
goNext (BreathingSession session) =
    let
        phases =
            List.drop 1 session.phases
    in
    BreathingSession
        { session
            | phases = phases
            , currentCycle =
                if List.head phases == Just Route.Path.Phases_Breathing then
                    session.currentCycle + 1

                else
                    session.currentCycle
        }


jumpToEnd : BreathingSession -> BreathingSession
jumpToEnd (BreathingSession session) =
    BreathingSession
        { session
            | phases = List.drop (List.length session.phases - 1) session.phases
            , currentCycle = session.currentCycle
        }


speedMillis : BreathingSession -> Int
speedMillis (BreathingSession session) =
    -- These are the speeds of the official WHM App (as of August 2023)
    case session.breathingSpeed of
        Slow ->
            2500

        Normal ->
            1750

        Quick ->
            1375


breathCount : BreathingSession -> Int
breathCount (BreathingSession session) =
    case session.breathCount of
        Thirty ->
            30

        Forty ->
            40

        Fifty ->
            50


relaxRetDuration : BreathingSession -> Int
relaxRetDuration (BreathingSession session) =
    session.relaxRetentionDuration


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



-- estimatedDuration :BreathingSession -> Int
-- estimatedDuration (BreathingSession session) =
