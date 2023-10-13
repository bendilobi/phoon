port module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute, loadExternalUrl
    , map, toCmd
    , adjustToday, navigate, navigateNext, playSound, resultsUpdated, saveMotivationData, saveSessionSettings, saveUpdatingState, sessionEnded, sessionUpdated, setUpdating, setWakeLock, soundEncoder, updateSessionSettings
    )

{-|

@docs Effect
@docs none, batch
@docs sendCmd, sendMsg
@docs pushRoute, replaceRoute, loadExternalUrl

@docs map, toCmd

-}

import Browser.Navigation
import Date
import Dict exposing (Dict)
import Json.Encode
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.Session as Session exposing (Session)
import Lib.SessionResults exposing (SessionResults)
import Lib.Utils as Utils
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Time
import Url exposing (Url)


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg
      -- PORTS
    | SendMessageToJavaScript
        { tag : String
        , data : Json.Encode.Value
        }



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


{-| Send a message as an effect. Useful when emitting events from UI components.
-}
sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- ROUTING


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute :
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }
    -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


{-| Redirect users to a new URL, somewhere external your web application.
-}
loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl



-- PORTS


port outgoing : { tag : String, data : Json.Encode.Value } -> Cmd msg


playSound : Utils.SessionSound -> Effect msg
playSound sound =
    SendMessageToJavaScript
        { tag = "PLAY_SOUND"
        , data = soundEncoder sound
        }


soundEncoder : Utils.SessionSound -> Json.Encode.Value
soundEncoder sound =
    let
        audioPath =
            "/audio/"
    in
    case sound of
        Utils.SessionStart ->
            Json.Encode.string <| audioPath ++ "ding.mp3"

        Utils.Breathing ->
            Json.Encode.string <| audioPath ++ "breathing.mp3"

        Utils.Retention ->
            Json.Encode.string <| audioPath ++ "retention.mp3"

        Utils.RelaxRetention ->
            Json.Encode.string <| audioPath ++ "relaxRetention.mp3"

        Utils.SessionEnd ->
            Json.Encode.string <| audioPath ++ "sessionEnd.mp3"


setWakeLock : Effect msg
setWakeLock =
    SendMessageToJavaScript
        { tag = "SET_WAKE_LOCK"
        , data = Json.Encode.string ""
        }


saveMotivationData : MotivationData -> Effect msg
saveMotivationData motData =
    SendMessageToJavaScript
        { tag = "STORE_MOTIVATION_DATA"
        , data = MotivationData.encoder motData
        }


saveSessionSettings : Session.Settings -> Effect msg
saveSessionSettings settings =
    SendMessageToJavaScript
        { tag = "STORE_SESSION_SETTINGS"
        , data = Session.settingsEncoder settings
        }


saveUpdatingState : Bool -> Effect msg
saveUpdatingState isUpdating =
    SendMessageToJavaScript
        { tag = "SET_UPDATING"
        , data = Json.Encode.bool isUpdating
        }



-- SHARED


setUpdating : Bool -> Effect msg
setUpdating isUpdating =
    SendSharedMsg <| Shared.Msg.SetUpdating isUpdating


adjustToday : Date.Date -> Effect msg
adjustToday today =
    SendSharedMsg <| Shared.Msg.AdjustToday today



--TODO: Doch nochmal explorieren, ob ich das irgendwie in einen
--      Schritt bekomme:
-- adjustToday : Effect Shared.Msg.Msg
-- adjustToday =
--     SendCmd <| Task.perform Shared.Msg.AdjustToday Date.today


sessionUpdated : Session -> Effect msg
sessionUpdated session =
    SendSharedMsg <| Shared.Msg.SessionUpdated session


resultsUpdated : SessionResults -> Effect msg
resultsUpdated results =
    SendSharedMsg <| Shared.Msg.ResultsUpdated results


navigate : Route.Path.Path -> Effect msg
navigate path =
    SendSharedMsg <| Shared.Msg.NavigateTriggered path


navigateNext : Session -> Effect msg
navigateNext session =
    case Session.goNext session of
        Just sess ->
            batch
                [ sessionUpdated sess
                , navigate <| Session.phasePath <| Session.currentPhase sess
                ]

        Nothing ->
            none


sessionEnded : Bool -> Effect msg
sessionEnded wasCancelled =
    --TODO: Statt Bool einen Typ in Session? type EndState = Successful | Cancelled
    SendSharedMsg <| Shared.Msg.SessionEnded wasCancelled


updateSessionSettings : Session.Settings -> Effect msg
updateSessionSettings settings =
    SendSharedMsg <| Shared.Msg.SessionSettingsUpdated settings



-- hiddenAt : Time.Posix -> Effect msg
-- hiddenAt time =
--     SendSharedMsg <| Shared.Msg.HiddenAt time
-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        LoadExternalUrl url ->
            LoadExternalUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg

        SendMessageToJavaScript msg ->
            SendMessageToJavaScript msg


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        LoadExternalUrl url ->
            Browser.Navigation.load url

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg

        SendMessageToJavaScript msg ->
            outgoing msg
