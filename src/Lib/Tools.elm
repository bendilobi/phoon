module Lib.Tools exposing (navigate)

import Dict
import Effect exposing (Effect)
import Route.Path exposing (Path)


navigate : Route.Path.Path -> Effect msg
navigate path =
    Effect.replaceRoute
        { path = path
        , query = Dict.empty
        , hash = Nothing
        }
