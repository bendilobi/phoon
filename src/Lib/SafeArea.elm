module Lib.SafeArea exposing (SafeArea, blanco, decode, maxX, new, paddingEach, paddingX)

import Json.Decode


type alias Edges =
    { top : Int, bottom : Int, left : Int, right : Int }


type SafeArea
    = Settings Edges


new : Edges -> SafeArea
new edges =
    Settings edges


blanco : SafeArea
blanco =
    Settings { top = 0, bottom = 0, left = 0, right = 0 }



--- ACCESS ---


paddingX : SafeArea -> Edges
paddingX (Settings settings) =
    { settings | top = 0, bottom = 0, right = settings.left }


maxX : SafeArea -> Int
maxX (Settings settings) =
    max settings.left settings.right


paddingEach : SafeArea -> Edges
paddingEach (Settings settings) =
    settings



--- DECODER ---


decoder : Json.Decode.Decoder SafeArea
decoder =
    let
        extractSafeAreaSize : String -> Int
        extractSafeAreaSize string =
            string
                |> String.split "px"
                |> List.head
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0
                --- It seems Apple adds 15 pixels to the actual size of the notch (iPhone XR...)
                --- This seems to be due to the "island" being more spacious than the notch, since
                --- it looks fine on iPhone 15
                |> max 0

        sizeExtractor : String -> String -> String -> String -> SafeArea
        sizeExtractor top bottom left right =
            Settings <|
                Edges
                    (top |> extractSafeAreaSize)
                    (bottom |> extractSafeAreaSize)
                    (left |> extractSafeAreaSize)
                    (right |> extractSafeAreaSize)
    in
    Json.Decode.map4 sizeExtractor
        (Json.Decode.field "sat" Json.Decode.string)
        (Json.Decode.field "sab" Json.Decode.string)
        (Json.Decode.field "sal" Json.Decode.string)
        (Json.Decode.field "sar" Json.Decode.string)


decode : Json.Decode.Value -> SafeArea
decode value =
    let
        decoded =
            Json.Decode.decodeValue decoder value
    in
    case decoded of
        Err e ->
            Settings <| Edges 0 0 0 0

        Ok sa ->
            sa
