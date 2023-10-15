module Shared.Model exposing (Model)

import Date
import Lib.ColorScheme as CS
import Lib.MotivationData exposing (MotivationData)
import Lib.Session as Session exposing (Session)
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
    , today : Date.Date
    , windowSize : { width : Int, height : Int }
    , session : Session
    , results : SessionResults
    , previousPath : Route.Path.Path
    , motivationData : MotivationData
    , colorScheme : CS.ColorScheme
    , sessionSettings : Session.Settings
    , appIsUpdating : Bool
    , justUpdated : Bool

    -- , safeAreaInsetBottom : String
    }
