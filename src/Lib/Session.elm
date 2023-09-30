module Lib.Session exposing
    ( BreathCount(..)
    , BreathingSpeed(..)
    , Phase(..)
    , Session
    , Settings
    , breathCount
    , breathCountChoices
    , breathCountInt
    , breathingSpeedDE
    , breathingSpeeds
    , currentPath
    , currentPhase
    , estimatedDuration
    , goNext
    , jumpToEnd
    , new
    , phasePath
    , relaxRetDuration
    , remainingCycles
    , settingsDecoder
    , settingsEncoder
    , speedMillis
    , withCycles
    , withFiftyBreaths
    , withRelaxRetDuration
    , withSpeedQuick
    , withSpeedSlow
    , withThirtyBreaths
    )

import Json.Decode
import Json.Encode
import Route.Path


type BreathingSpeed
    = Slow
    | Medium
    | Fast


breathingSpeeds : List BreathingSpeed
breathingSpeeds =
    let
        ignored thing =
            case thing of
                Slow ->
                    ()

                Medium ->
                    ()

                Fast ->
                    ()

        -- If the compiler complains, maybe add the missing thing here?
    in
    [ Slow, Medium, Fast ]


breathingSpeedDE : BreathingSpeed -> String
breathingSpeedDE speed =
    case speed of
        Slow ->
            "Langsam"

        Medium ->
            "Mittel"

        Fast ->
            "Schnell"


breathingSpeedEN : BreathingSpeed -> String
breathingSpeedEN speed =
    case speed of
        Slow ->
            "Slow"

        Medium ->
            "Medium"

        Fast ->
            "Fast"


type BreathCount
    = Thirty
    | Forty
    | Fifty


breathCountChoices : List BreathCount
breathCountChoices =
    let
        ignored thing =
            case thing of
                Thirty ->
                    ()

                Forty ->
                    ()

                Fifty ->
                    ()
    in
    [ Thirty, Forty, Fifty ]



--TODO: Brauche ich das oder kann ich unten die session-bezogene Funktion umbauen?
-- breathCountDE : BreathCount -> String
-- breathCountDE bc =
--     case bc of
--         Thirty ->


breathCountInt : BreathCount -> Int
breathCountInt bc =
    case bc of
        Thirty ->
            30

        Forty ->
            40

        Fifty ->
            50


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
    , breathingSpeed : BreathingSpeed
    , breathCount : BreathCount
    }


new :
    Settings
    -> Session
new props =
    Session
        { state = createState props.cycles
        , breathCount = props.breathCount
        , breathingSpeed = props.breathingSpeed
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
    Session { session | breathingSpeed = Fast }


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

        Medium ->
            1750

        Fast ->
            1375


breathCount : Session -> Int
breathCount (Session session) =
    breathCountInt session.breathCount



-- case session.breathCount of
--     Thirty ->
--         30
--     Forty ->
--         40
--     Fifty ->
--         50


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



-- CONVERSION


fieldnames :
    { cycles : String
    , relaxRetDuration : String
    , breathingSpeed : String
    , breathCount : String
    }
fieldnames =
    { cycles = "cycles"
    , relaxRetDuration = "relaxRetDur"
    , breathingSpeed = "breathSpeed"
    , breathCount = "breathCount"
    }


breathingSpeedStrings =
    { slow = "slow"
    , medium = "medium"
    , fast = "fast"
    }


settingsEncoder : Settings -> Json.Encode.Value
settingsEncoder settings =
    Json.Encode.object
        [ ( fieldnames.cycles, Json.Encode.int settings.cycles )
        , ( fieldnames.relaxRetDuration, Json.Encode.int settings.relaxRetDuration )
        , ( fieldnames.breathingSpeed, Json.Encode.string <| breathingSpeedToString settings.breathingSpeed )
        , ( fieldnames.breathCount, Json.Encode.int <| breathCountInt settings.breathCount )
        ]


breathingSpeedToString : BreathingSpeed -> String
breathingSpeedToString speed =
    case speed of
        Slow ->
            breathingSpeedStrings.slow

        Medium ->
            breathingSpeedStrings.medium

        Fast ->
            breathingSpeedStrings.fast


breathingSpeedFromString : String -> Json.Decode.Decoder BreathingSpeed
breathingSpeedFromString string =
    if string == breathingSpeedStrings.slow then
        Json.Decode.succeed Slow

    else if string == breathingSpeedStrings.medium then
        Json.Decode.succeed Medium

    else if string == breathingSpeedStrings.fast then
        Json.Decode.succeed Fast

    else
        Json.Decode.fail ("Invalid breathing speed value: " ++ string)


breathingSpeedDecoder : Json.Decode.Decoder BreathingSpeed
breathingSpeedDecoder =
    Json.Decode.string
        |> Json.Decode.andThen breathingSpeedFromString


breathCountDecoder : Json.Decode.Decoder BreathCount
breathCountDecoder =
    Json.Decode.int
        |> Json.Decode.andThen
            (\count ->
                case count of
                    30 ->
                        Json.Decode.succeed Thirty

                    40 ->
                        Json.Decode.succeed Forty

                    50 ->
                        Json.Decode.succeed Fifty

                    _ ->
                        Json.Decode.fail ("Invalid breath count value: " ++ String.fromInt count)
            )


settingsDecoder : Json.Decode.Decoder Settings
settingsDecoder =
    Json.Decode.map4
        Settings
        (Json.Decode.field fieldnames.cycles Json.Decode.int)
        (Json.Decode.field fieldnames.relaxRetDuration Json.Decode.int)
        (Json.Decode.field fieldnames.breathingSpeed breathingSpeedDecoder)
        (Json.Decode.field fieldnames.breathCount breathCountDecoder)
