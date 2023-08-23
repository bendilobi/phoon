module Shared.Model exposing (Model)

import Lib.Session exposing (Session)
import Lib.SessionResults exposing (SessionResults)
import Route.Path
import Time


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , session : Session
    , results : SessionResults
    , previousPath : Route.Path.Path
    }
