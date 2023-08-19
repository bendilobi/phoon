module Shared.Msg exposing (Msg(..))

{-| -}

import Lib.BreathingSession exposing (BreathingSession)
import Lib.SessionResults exposing (SessionResults)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = SessionUpdated BreathingSession
    | ResultsUpdated SessionResults
