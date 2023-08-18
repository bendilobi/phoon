module Lib.Tools exposing (navigate, navigateNext)

import Dict
import Effect exposing (Effect)
import Lib.BreathingSession as BS exposing (BreathingSession)
import Route.Path exposing (Path)


navigate : Route.Path.Path -> Effect msg
navigate path =
    Effect.replaceRoute
        { path = path
        , query = Dict.empty
        , hash = Nothing
        }


navigateNext : BreathingSession -> Effect msg
navigateNext session =
    let
        newSession =
            BS.goNext session
    in
    Effect.batch
        [ Effect.sessionUpdated newSession
        , navigate <| BS.currentPath newSession
        ]
