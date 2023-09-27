module Lib.Session exposing
    ( Phase(..)
    , Session
    , Settings
    , breathCount
    , currentPath
    , currentPhase
    , estimatedDuration
    , goNext
    , jumpToEnd
    , new
    , phasePath
    , relaxRetDuration
    , remainingCycles
    , speedMillis
    , withCycles
    , withFiftyBreaths
    , withRelaxRetDuration
    , withSpeedQuick
    , withSpeedSlow
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


type Phase
    = Start
    | Breathing
    | Retention
    | RelaxRetention
    | End


type SessionState
    = State Phase (List Phase)


type Session
    = Session
        { state : SessionState
        , breathCount : BreathCount
        , breathingSpeed : BreathingSpeed
        , relaxRetentionDuration : Int
        }


type alias Settings =
    { cycles : Int
    , relaxRetDuration : Int
    }


new :
    Settings
    -> Session
new props =
    Session
        { state = createState props.cycles
        , breathCount = Forty
        , breathingSpeed = Normal
        , relaxRetentionDuration = props.relaxRetDuration
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


withCycles : Int -> Session -> Session
withCycles numberOfCycles (Session session) =
    Session { session | state = createState numberOfCycles }


withThirtyBreaths : Session -> Session
withThirtyBreaths (Session session) =
    Session { session | breathCount = Thirty }


withFiftyBreaths : Session -> Session
withFiftyBreaths (Session session) =
    Session { session | breathCount = Fifty }


withSpeedSlow : Session -> Session
withSpeedSlow (Session session) =
    Session { session | breathingSpeed = Slow }


withSpeedQuick : Session -> Session
withSpeedQuick (Session session) =
    Session { session | breathingSpeed = Quick }


withRelaxRetDuration : Int -> Session -> Session
withRelaxRetDuration dur (Session session) =
    Session { session | relaxRetentionDuration = dur }


goNext : Session -> Maybe Session
goNext (Session session) =
    let
        (State _ remainingPhases) =
            session.state
    in
    List.head remainingPhases
        |> Maybe.map
            (\phase ->
                Session
                    { session
                        | state = State phase <| List.drop 1 remainingPhases
                    }
            )


jumpToEnd : Session -> Session
jumpToEnd (Session session) =
    Session
        { session
            | state = State End []
        }


remainingCycles : Session -> Int
remainingCycles (Session session) =
    let
        (State _ upcomingPhases) =
            session.state
    in
    (List.length upcomingPhases - 1) // 3


speedMillis : Session -> Int
speedMillis (Session session) =
    -- These are the speeds of the official WHM App (as of August 2023)
    case session.breathingSpeed of
        Slow ->
            2500

        Normal ->
            1750

        Quick ->
            1375


breathCount : Session -> Int
breathCount (Session session) =
    case session.breathCount of
        Thirty ->
            30

        Forty ->
            40

        Fifty ->
            50


relaxRetDuration : Session -> Int
relaxRetDuration (Session session) =
    session.relaxRetentionDuration


currentPath : Session -> Route.Path.Path
currentPath session =
    phasePath <| currentPhase session


currentPhase : Session -> Phase
currentPhase (Session session) =
    let
        (State current _) =
            session.state
    in
    current



-- TODO: Eigenen Typ für Zeit definieren? type Duration = Seconds | Millis?
--       Sodass in Signaturen klar ist, um welche Einheit es geht?
-- https://package.elm-lang.org/packages/jxxcarlson/elm-typed-time/latest/


phaseDuration : Session -> Phase -> Int
phaseDuration session phase =
    case phase of
        Start ->
            5000

        Breathing ->
            speedMillis session * 2 * breathCount session

        Retention ->
            -- TODO: Stattdessen aus vergangenen Sessions ermitteln
            2 * 60000 + 15000

        RelaxRetention ->
            relaxRetDuration session * 1000

        End ->
            2 * 60000


estimatedDuration : Session -> Int
estimatedDuration (Session session) =
    let
        (State curPhase remainingPhases) =
            session.state
    in
    (curPhase :: remainingPhases)
        |> List.map (phaseDuration <| Session session)
        |> List.sum
