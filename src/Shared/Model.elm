module Shared.Model exposing (InfoWindowState(..), Model, UpdateState(..))

import Api
import Date
import Element exposing (Color)
import Lib.ColorScheme as CS
import Lib.MotivationData exposing (MotivationData)
import Lib.PageFading as Fading
import Lib.SafeArea exposing (SafeArea)
import Lib.Session as Session exposing (Session)
import Lib.SessionResults exposing (SessionResults)
import Lib.Texts as Texts
import Lib.Utils as Utils
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
    , appLanguage : Texts.AppLanguage

    {- This is Nothing in browsers other than Safari, False in
       in Safari if not "installed" to the home screen and True if installed.
    -}
    , standalone : Maybe Bool

    {- We go under the assumption that if a mouse is used, the app is run in a desktop environment with keyboard.
       So we use this field for desktop-specific stuff:
    -}
    , pointerIsMouse : Maybe Bool
    , iOSVersion : Maybe Int
    , appVisible : Bool
    , updateState : UpdateState
    , showWakelockNote : Bool
    , versionOnServer : Api.Data String
    , deviceInfo : Utils.Device
    , session : Session
    , results : SessionResults
    , previousPath : Route.Path.Path
    , motivationData : Maybe MotivationData
    , previousMotivationData : Maybe MotivationData
    , colorScheme : CS.ColorScheme
    , sessionSettings : Session.Settings
    , baseApiUrl : String
    , safeAreaInset : SafeArea
    , fadeIn : Fading.Trigger
    , infoWindowState : InfoWindowState
    , sessionHintsHeight : Maybe Float
    , subPageShown : Bool
    , subPageClosingInProgress : Bool
    }


type InfoWindowState
    = Closed
    | Half
    | Max


type UpdateState
    = NotUpdating
    | UpdateAvailable String
    | Updating Int
    | JustUpdated
    | UpdateFailed String
