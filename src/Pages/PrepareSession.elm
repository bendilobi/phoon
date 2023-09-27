module Pages.PrepareSession exposing (Model, Msg, page)

import Components.Button
import Components.CrementButton
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
    Layouts.MainNav {}



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
    | AddCyclePressed
    | RemoveCyclePressed


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

        AddCyclePressed ->
            ( model
            , shared.session
                |> Session.withCycles (Session.remainingCycles shared.session + 1)
                |> Effect.sessionUpdated
            )

        RemoveCyclePressed ->
            ( model
            , shared.session
                |> Session.withCycles (Session.remainingCycles shared.session - 1)
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
            , height fill
            ]
            [ el
                ([ width fill
                 , Font.center
                 , Font.bold
                 , padding 10

                 --  , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
                 --  , Border.color <| rgb255 34 33 31
                 ]
                    ++ CS.primaryMotivation shared.colorScheme
                )
              <|
                text "Sitzung vorbereiten"
            , column
                [ width fill
                , padding 20
                , Font.center
                , spacing 70
                , centerY
                ]
                [ column [ centerX, spacing 30 ]
                    [ row
                        [ centerX
                        , spacing 20
                        , Font.size 20
                        ]
                        [ Components.CrementButton.new
                            { onPress = RemoveCyclePressed
                            , crement = Components.CrementButton.De
                            }
                            |> Components.CrementButton.withDisabled (Session.remainingCycles shared.session == 1)
                            |> Components.CrementButton.view shared.colorScheme
                        , row []
                            [ el [ Font.bold ] <| text <| String.fromInt <| Session.remainingCycles shared.session
                            , text " Runde"
                            , el [ transparent <| Session.remainingCycles shared.session == 1 ] <| text "n"
                            ]
                        , Components.CrementButton.new
                            { onPress = AddCyclePressed
                            , crement = Components.CrementButton.In
                            }
                            |> Components.CrementButton.withDisabled (Session.remainingCycles shared.session == 10)
                            |> Components.CrementButton.view shared.colorScheme
                        ]
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
            ]
    }


viewEstimatedTime : Session -> Time.Zone -> Time.Posix -> Element msg
viewEstimatedTime session zone time =
    let
        estimate =
            time
                |> Time.posixToMillis
                |> (+) (Session.estimatedDuration session)
                |> Time.millisToPosix

        hour =
            String.fromInt <| Time.toHour zone estimate

        minute =
            Time.toMinute zone estimate
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    text <| hour ++ ":" ++ minute
