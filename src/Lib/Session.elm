module Lib.Session exposing
    ( BreathCount(..)
    , BreathingSpeed(..)
    , Phase(..)
    , Session
    , SessionSound(..)
    , Settings
    , breathCount
    , breathCountChoices
    , breathCountInt
    , breathingSpeedDE
    , breathingSpeeds
    , currentPath
    , currentPhase
    , defaultSettings
    , estimatedDurationMillis
    , goNext
    , jumpToEnd
    , new
    , phasePath
    , relaxRetDuration
    , remainingCycles
    , settingsDecoder
    , settingsEncoder
    , speedMillis
    , speedToMillis
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
    = Twenty
    | Thirty
    | Forty
    | Fifty


breathCountChoices : List BreathCount
breathCountChoices =
    let
        ignored thing =
            case thing of
                Twenty ->
                    ()

                Thirty ->
                    ()

                Forty ->
                    ()

                Fifty ->
                    ()
    in
    [ Twenty, Thirty, Forty, Fifty ]


breathCountInt : BreathCount -> Int
breathCountInt bc =
    case bc of
        Twenty ->
            20

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


type SessionSound
    = StartSound
    | BreathingSound
    | RetentionSound
    | RelaxRetentionSound
    | EndSound


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


defaultSettings : Settings
defaultSettings =
    { cycles = 3
    , relaxRetDuration = 15
    , breathingSpeed = Medium
    , breathCount = Thirty
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
    speedToMillis session.breathingSpeed


speedToMillis : BreathingSpeed -> Int
speedToMillis speed =
    -- These are the speeds of the official WHM App (as of August 2023)
    case speed of
        Slow ->
            2500

        Medium ->
            1750

        Fast ->
            1375


breathCount : Session -> Int
breathCount (Session session) =
    breathCountInt session.breathCount


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


phaseDuration : Session -> Maybe Int -> Phase -> Int
phaseDuration session retentionEstimate phase =
    case phase of
        Start ->
            5000

        Breathing ->
            speedMillis session * 2 * breathCount session

        Retention ->
            case retentionEstimate of
                Nothing ->
                    --- Two minutes and 15 seconds...
                    2 * 60000 + 15000

                Just estimate ->
                    estimate

        RelaxRetention ->
            relaxRetDuration session * 1000

        End ->
            2 * 60000


estimatedDurationMillis : List Int -> Session -> Int
estimatedDurationMillis meanRetTimes (Session session) =
    let
        (State curPhase remainingPhases) =
            session.state

        retentionEstimate =
            if List.length meanRetTimes == 0 then
                Nothing

            else
                Just <| (List.sum meanRetTimes // List.length meanRetTimes) * 1000
    in
    (curPhase :: remainingPhases)
        |> List.map (phaseDuration (Session session) retentionEstimate)
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
    let
        ignored thing =
            case thing of
                Twenty ->
                    ()

                Thirty ->
                    ()

                Forty ->
                    ()

                Fifty ->
                    ()

        --- If the compiler complains here, adjust the pattern matched below
        --- to include all cases!
    in
    Json.Decode.int
        |> Json.Decode.andThen
            (\count ->
                case count of
                    20 ->
                        Json.Decode.succeed Twenty

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
