module Api exposing (Data(..), failureToString)

import Http


type Data value
    = Loading
    | Success value
    | Failure Http.Error


failureToString : Http.Error -> String
failureToString err =
    case err of
        Http.BadUrl url ->
            url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus i ->
            "Bad Status: " ++ String.fromInt i

        Http.BadBody b ->
            b
