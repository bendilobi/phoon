module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    , navigateNext
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Effect exposing (Effect)
import Json.Decode
import Lib.BreathingSession as BreathingSession exposing (BreathingSession)
import Lib.SessionResults as SR
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed {}



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    ( { session = BreathingSession.new
      , results = SR.empty
      }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.SessionUpdated session ->
            ( { model | session = session }
            , Effect.none
            )

        Shared.Msg.ResultsUpdated results ->
            ( { model | results = results }
            , Effect.none
            )


navigateNext : BreathingSession -> Effect msg
navigateNext session =
    let
        ( newSession, path ) =
            case BreathingSession.goNext session of
                Nothing ->
                    ( BreathingSession.new, Route.Path.Home_ )

                Just sess ->
                    ( sess, BreathingSession.currentPath sess )
    in
    Effect.batch
        [ Effect.sessionUpdated newSession
        , Effect.navigate <| path
        ]



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
