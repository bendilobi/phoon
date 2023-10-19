port module Effect exposing
    ( Effect
    , none, batch
    , sendCmd, sendMsg
    , pushRoute, replaceRoute, loadExternalUrl
    , map, toCmd
    , adjustToday, checkVersion, clipboardReceiver, getSafeArea, navigate, navigateNext, playSound, receivedVersionOnServer, reload, requestClipboardContent, resultsUpdated, safeAreaReceiver, saveMotivationData, saveSessionSettings, saveUpdatingState, sessionEnded, sessionUpdated, setMotivationData, setUpdating, setWakeLock, soundEncoder, updateApp, updateSessionSettings, writeToClipboard
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
import Http
import Json.Decode
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
    | Reload
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg
      -- PORTS
    | SendMessageToJavaScript
        { tag : String
        , data : Json.Encode.Value
        }
    | SendApiRequest
        { endpoint : String
        , decoder : Json.Decode.Decoder msg
        , onHttpError : Http.Error -> msg
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


reload : Effect msg
reload =
    Reload



-- PORTS


port outgoing : { tag : String, data : Json.Encode.Value } -> Cmd msg



--- TODO: In der elm land Doku wird ein "incoming"-port empfohlen... Aber wie?


port safeAreaReceiver : (Json.Decode.Value -> msg) -> Sub msg


port clipboardReceiver : (Json.Decode.Value -> msg) -> Sub msg


writeToClipboard : String -> Effect msg
writeToClipboard text =
    SendMessageToJavaScript
        { tag = "CLIPBOARD_WRITE"
        , data = Json.Encode.string text
        }


requestClipboardContent : Effect msg
requestClipboardContent =
    SendMessageToJavaScript
        { tag = "REQUEST_CLIPBOARD"
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


getSafeArea : Effect msg
getSafeArea =
    SendMessageToJavaScript
        { tag = "GET_SAFE_AREA"
        , data = Json.Encode.string ""
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


receivedVersionOnServer : Result Http.Error String -> Effect msg
receivedVersionOnServer result =
    SendSharedMsg <| Shared.Msg.ReceivedVersionOnServer result


setMotivationData : MotivationData -> Effect msg
setMotivationData motData =
    SendSharedMsg <| Shared.Msg.SetMotivationData motData



--- Commands ---


updateApp : Effect msg
updateApp =
    batch
        [ setUpdating True
        , Reload
        ]



--- API ---


checkVersion : (Result Http.Error String -> msg) -> Effect msg
checkVersion msg =
    sendApiRequest
        { endpoint = "version.json"
        , decoder = versionDecoder
        , onResponse = msg
        }


versionDecoder : Json.Decode.Decoder String
versionDecoder =
    Json.Decode.field "version" <| Json.Decode.string


sendApiRequest :
    { endpoint : String
    , decoder : Json.Decode.Decoder value
    , onResponse : Result Http.Error value -> msg
    }
    -> Effect msg
sendApiRequest options =
    let
        decoder : Json.Decode.Decoder msg
        decoder =
            options.decoder
                |> Json.Decode.map Ok
                |> Json.Decode.map options.onResponse

        onHttpError : Http.Error -> msg
        onHttpError httpError =
            options.onResponse (Err httpError)
    in
    SendApiRequest
        { endpoint = options.endpoint
        , decoder = decoder
        , onHttpError = onHttpError
        }



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

        Reload ->
            Reload

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg

        SendMessageToJavaScript msg ->
            SendMessageToJavaScript msg

        SendApiRequest data ->
            SendApiRequest
                { endpoint = data.endpoint
                , decoder = Json.Decode.map fn data.decoder
                , onHttpError = \err -> fn (data.onHttpError err)
                }


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

        Reload ->
            Browser.Navigation.reload

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg

        SendMessageToJavaScript msg ->
            outgoing msg

        SendApiRequest data ->
            Http.request
                { method = "GET"
                , url = options.shared.baseApiUrl ++ data.endpoint
                , headers =
                    -- case options.shared.user of
                    --     Just user ->
                    --         [ Http.header
                    --             "Authorization"
                    --             ("Bearer " ++ user.token)
                    --         ]
                    --     Nothing ->
                    []
                , body = Http.emptyBody
                , expect =
                    Http.expectJson
                        (\httpResult ->
                            case httpResult of
                                Ok msg ->
                                    msg

                                Err httpError ->
                                    data.onHttpError httpError
                        )
                        data.decoder
                , timeout = Just 15000
                , tracker = Nothing
                }
