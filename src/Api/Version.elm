module Api.Version exposing (getNewest)

import Http
import Json.Decode


getNewest : { onResponse : Result Http.Error String -> msg } -> Cmd msg
getNewest options =
    Http.get
        --- TODO: URL deployment-unabhängig erzeugen
        { url = "https://zoff.benno-dielmann.de/version.json"
        , expect = Http.expectJson options.onResponse decoder
        }


decoder : Json.Decode.Decoder String
decoder =
    Json.Decode.field "version" <| Json.Decode.string
