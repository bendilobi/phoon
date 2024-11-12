module Shared.Msg exposing (Msg(..))

{-| -}

import Browser.Dom
import Browser.Events
import Date
import Http
import Json.Decode
import Lib.MotivationData exposing (MotivationData)
import Lib.PageFading as Fading
import Lib.Session as Session exposing (Session)
import Lib.SessionResults exposing (SessionResults)
import Route.Path
import Shared.Model
import Time


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type Msg
    = ReceivedViewport Browser.Dom.Viewport
    | Resized Int Int
    | RequestSafeArea
    | ReceivedSafeArea Json.Decode.Value
    | VisibilityChanged Browser.Events.Visibility
    | AdjustTimeZone Time.Zone
    | GetToday
    | AdjustToday Date.Date
    | SessionUpdated Session
    | CancelSession Session
    | ResultsUpdated SessionResults
    | NavigateTriggered Fading.Trigger Route.Path.Path
    | SessionEnded Session.EndType
    | SessionSettingsUpdated Session.Settings
    | SetUpdateState Shared.Model.UpdateState
    | ReceivedVersionOnServer (Result Http.Error String)
    | SetMotivationData (Maybe MotivationData)
    | SetInfoWindowState Shared.Model.InfoWindowState
    | SessionHintsHeightRequested
    | ReceivedSessionHintsElement (Result Browser.Dom.Error Browser.Dom.Element)
    | PrepareToggleSubPage
    | OnToggleSubPage
    | PointerDevice Bool
    | OnToggleShowWakelockHint
