port module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute, loadExternalUrl
    , map, toCmd
    , navigate, playSound, reloadApp, resultsUpdated, saveMotivationData, sessionEnded, sessionUpdated, setWakeLock, soundEncoder
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


reloadApp : Effect msg
reloadApp =
    SendMessageToJavaScript
        { tag = "RELOAD_APP"
        , data = Json.Encode.string ""
        }


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



-- SHARED


sessionUpdated : Session -> Effect msg
sessionUpdated session =
    SendSharedMsg <| Shared.Msg.SessionUpdated session


resultsUpdated : SessionResults -> Effect msg
resultsUpdated results =
    SendSharedMsg <| Shared.Msg.ResultsUpdated results


navigate : Route.Path.Path -> Effect msg
navigate path =
    SendSharedMsg <| Shared.Msg.NavigateTriggered path



-- sessionEnded : Date.Date -> Effect msg
-- sessionEnded today =
--     SendSharedMsg <| Shared.Msg.SessionEnded today


sessionEnded : Effect msg
sessionEnded =
    SendSharedMsg <| Shared.Msg.SessionEndedX



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
