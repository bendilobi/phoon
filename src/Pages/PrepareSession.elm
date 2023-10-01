module Pages.PrepareSession exposing (Model, Msg, page)

import Components.Button
import Components.IntCrementer as IntCrementer
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Session as Session exposing (Session)
import Lib.SessionResults as SessionResults
import Lib.Utils as Utils
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav { header = Just "Sitzung vorbereiten" }



-- INIT


type alias Model =
    { time : Time.Posix
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { time = Time.millisToPosix 0
      }
    , Effect.sendCmd <| Task.perform Tick Time.now
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SessionStartPressed
    | CycleCountChanged Int


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Effect.none
            )

        SessionStartPressed ->
            ( model
            , Effect.batch
                [ Effect.resultsUpdated SessionResults.empty
                , Effect.playSound Utils.SessionStart
                , Effect.navigate <|
                    Session.currentPath shared.session
                ]
            )

        CycleCountChanged cycles ->
            ( model
            , shared.session
                |> Session.withCycles cycles
                |> Effect.sessionUpdated
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Session"
    , attributes = CS.primaryPrepareSession shared.colorScheme
    , element =
        column
            [ width fill
            , padding 20
            , Font.center
            , spacing 70
            , centerY
            ]
            [ column [ centerX, spacing 30 ]
                [ IntCrementer.new
                    { label =
                        \n ->
                            row []
                                [ el [ Font.bold ] <| text <| String.fromInt n
                                , text " Runde"
                                , el [ transparent <| n == 1 ] <| text "n"
                                ]
                    , onCrement = CycleCountChanged
                    }
                    |> IntCrementer.withMin 1
                    |> IntCrementer.withMax 9
                    |> IntCrementer.view shared.colorScheme (Session.remainingCycles shared.session)
                , paragraph []
                    [ text "Geschätztes Ende: "
                    , el [ Font.bold, Font.size 30 ] <| viewEstimatedTime shared.session shared.zone model.time
                    , text " Uhr"
                    ]
                ]
            , el [ width fill ]
                (Components.Button.new
                    { onPress = Just SessionStartPressed
                    , label = text "Los geht's!"
                    }
                    |> Components.Button.view shared.colorScheme
                )
            ]
    }


viewEstimatedTime : Session -> Time.Zone -> Time.Posix -> Element msg
viewEstimatedTime session zone time =
    let
        estimate =
            time
                |> Time.posixToMillis
                |> (+) (Session.estimatedDurationMillis session)
                |> Time.millisToPosix

        hour =
            String.fromInt <| Time.toHour zone estimate

        minute =
            Time.toMinute zone estimate
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    text <| hour ++ ":" ++ minute
