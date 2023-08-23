module Shared.Msg exposing (Msg(..))

{-| -}

import Lib.Session exposing (Session)
import Lib.SessionResults exposing (SessionResults)
import Route.Path
import Time


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | SessionUpdated Session
    | ResultsUpdated SessionResults
    | NavigateTriggered Route.Path.Path
