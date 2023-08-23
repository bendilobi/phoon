module Lib.BreathingSession exposing
    ( BreathingSession
    , Phase(..)
    , addCycle
    , breathCount
    , currentCycle
    , currentPath
    , estimatedDuration
    , goNext
    , jumpToEnd
    , new
    , phasePath
    , relaxRetDuration
    , speedMillis
    , withCycles
    , withFiftyBreaths
    , withRelaxRetDuration
    , withSpeedQuick
    , withSpeedSlow
    , withThirtyBreaths
    )

import Route.Path



-- TODO: Modul in "Session" umbenennen?


type BreathingSpeed
    = Slow
    | Normal
    | Quick


type BreathCount
    = Thirty
    | Forty
    | Fifty


type Phase
    = Start
    | Breathing
    | Retention
    | RelaxRetention
    | End


type SessionState
    = State Phase (List Phase)


type BreathingSession
    = BreathingSession
        { state : SessionState
        , currentCycle : Int
        , breathCount : BreathCount
        , breathingSpeed : BreathingSpeed
        , relaxRetentionDuration : Int
        }


new : BreathingSession
new =
    BreathingSession
        { state = createState 4
        , currentCycle = 0
        , breathCount = Forty
        , breathingSpeed = Normal
        , relaxRetentionDuration = 15
        }


phasePath : Phase -> Route.Path.Path
phasePath phase =
    case phase of
        Start ->
            Route.Path.Phases_SessionStart

        Breathing ->
            Route.Path.Phases_Breathing

        Retention ->
            Route.Path.Phases_Retention

        RelaxRetention ->
            Route.Path.Phases_RelaxRetention

        End ->
            Route.Path.Phases_SessionEnd


createState : Int -> SessionState
createState numberOfCycles =
    let
        cycle =
            [ Breathing
            , Retention
            , RelaxRetention
            ]
    in
    State Start <|
        (List.repeat numberOfCycles cycle
            |> List.concat
        )
            ++ [ End ]


withCycles : Int -> BreathingSession -> BreathingSession
withCycles numberOfCycles (BreathingSession session) =
    BreathingSession { session | state = createState numberOfCycles }


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



-- TODO: besser "continueSession"?


addCycle : BreathingSession -> BreathingSession
addCycle (BreathingSession session) =
    BreathingSession
        { session
            | state = createState 1
            , currentCycle = session.currentCycle
        }


goNext : BreathingSession -> Maybe BreathingSession
goNext (BreathingSession session) =
    let
        (State _ remainingPhases) =
            session.state
    in
    List.head remainingPhases
        |> Maybe.map
            (\phase ->
                BreathingSession
                    { session
                        | state = State phase <| List.drop 1 remainingPhases
                        , currentCycle =
                            if phase == Breathing then
                                session.currentCycle + 1

                            else
                                session.currentCycle
                    }
            )


jumpToEnd : BreathingSession -> BreathingSession
jumpToEnd (BreathingSession session) =
    BreathingSession
        { session
            | state = State End []

            --List.drop (List.length session.phases - 1) session.phases
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
    let
        (State current _) =
            session.state
    in
    phasePath current



-- List.head session.state
--     |> Maybe.map phasePath
-- List.head session.phases
--     |> phasePath
-- case List.head session.phases of
--     Nothing ->
--         -- TODO: Wenn der currentPath Home_ ist, sollte eine neue Session initialisiert werden
--         --       => wie und wo implementieren?
--         Route.Path.Home_
--     Just phase ->
--         phasePath phase


currentCycle : BreathingSession -> Int
currentCycle (BreathingSession session) =
    session.currentCycle



-- TODO: Eigenen Typ für Zeit definieren? type Duration = Seconds | Millis?
--       Sodass in Signaturen klar ist, um welche Einheit es geht?


phaseDuration : BreathingSession -> Phase -> Int
phaseDuration session phase =
    case phase of
        Start ->
            5

        Breathing ->
            (speedMillis session * 2 * breathCount session) // 1000

        Retention ->
            -- TODO: Stattdessen aus vergangenen Sessions ermitteln
            2 * 60

        RelaxRetention ->
            relaxRetDuration session

        End ->
            0


estimatedDuration : BreathingSession -> Int
estimatedDuration (BreathingSession session) =
    let
        (State currentPhase remainingPhases) =
            session.state
    in
    (currentPhase :: remainingPhases)
        |> List.map (phaseDuration <| BreathingSession session)
        |> List.sum
