module Pages.Phases.SessionEnd exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Session as Session
import Lib.SessionResults as SessionResults exposing (SessionResults)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.SessionControls
        { showCurrentCycle = Nothing
        , ifCancelled = Effect.sessionEnded Session.Cancelled
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.playSound Session.EndSound
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Session End"
    , attributes =
        CS.phaseSessionEnd shared.colorScheme
    , element =
        column
            [ width fill
            , height fill
            , spacing 30
            ]
            [ el
                [ centerX
                , centerY
                , Font.bold
                , Font.size 40
                ]
              <|
                text "Sitzung beendet!"
            , viewRetentionTimes shared.results
            ]
    }


viewRetentionTimes : SessionResults -> Element msg
viewRetentionTimes results =
    case SessionResults.getRetentionTimes results of
        Nothing ->
            none

        Just times ->
            column
                [ spacing 10
                , centerX
                , centerY
                , Font.alignRight
                ]
            <|
                List.map2
                    (\i t ->
                        row [ width fill ]
                            [ el [ width fill ] <| text <| "Runde " ++ String.fromInt i ++ ": "
                            , el [ Font.bold ] <| text <| formatRetentionTime t
                            ]
                    )
                    (List.range 1 (List.length times))
                    times
                    ++ [ row
                            [ width fill
                            , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                            , paddingXY 0 7
                            ]
                            [ el [ width fill ] <| text "Durchschnitt: "
                            , el
                                [ Font.bold
                                ]
                              <|
                                text <|
                                    (SessionResults.meanRetentionTime results
                                        |> Maybe.withDefault 0
                                        |> formatRetentionTime
                                    )
                            ]
                       ]


formatRetentionTime : Int -> String
formatRetentionTime seconds =
    String.join ":"
        [ String.padLeft 1 '0' <| String.fromInt <| remainderBy 60 (seconds // 60)
        , String.padLeft 2 '0' <| String.fromInt <| remainderBy 60 seconds
        ]
