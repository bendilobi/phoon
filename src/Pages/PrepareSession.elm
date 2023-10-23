module Pages.PrepareSession exposing (Model, Msg, page)

import Animator
import Components.AnimatedButton as Button
import Components.IntCrementer as IntCrementer
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Millis as Millis
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Lib.Session as Session exposing (Session)
import Lib.SessionResults as SessionResults
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Task
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav { header = Just "Sitzung vorbereiten" }



-- INIT


type alias Model =
    { time : Time.Posix
    , startButton : Button.Model Msg
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { time = Time.millisToPosix 0
      , startButton =
            Button.init
                { id = "start"
                , onPress = Just SessionStartPressed
                }
      }
    , Effect.batch
        [ Effect.sendCmd <| Task.perform Tick Time.now
        , Effect.sessionUpdated <| Session.new shared.sessionSettings
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AnimationTick Time.Posix
    | SessionStartPressed
    | CycleCountChanged Int
    | ButtonSent Button.Msg


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Effect.none
            )

        AnimationTick newTime ->
            Button.update
                { msg = Button.AnimationTick newTime
                , model = model.startButton
                , toModel = \button -> { model | startButton = button }
                }

        ButtonSent innerMsg ->
            Button.update
                { msg = innerMsg
                , model = model.startButton
                , toModel = \button -> { model | startButton = button }
                }

        SessionStartPressed ->
            ( model
            , Effect.batch
                [ Effect.resultsUpdated SessionResults.empty
                , Effect.playSound Session.StartSound
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


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    if shared.appVisible then
        Sub.batch
            [ Time.every 1000 Tick
            , Animator.toSubscription AnimationTick model.startButton Button.animator
            ]

    else
        Sub.none



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
                    , el [ Font.bold, Font.size 30 ] <| viewEstimatedTime shared model.time
                    , text " Uhr"
                    ]
                ]
            , el [ width fill ]
                (Button.new
                    { model = model.startButton

                    -- , onPress = Just SessionStartPressed
                    , label = text "Los geht's!"
                    , toMsg = ButtonSent
                    }
                    |> Button.view shared.colorScheme
                )
            ]
    }


viewEstimatedTime : Shared.Model -> Time.Posix -> Element msg
viewEstimatedTime shared time =
    let
        estimate =
            time
                |> Time.posixToMillis
                |> (+)
                    (Session.estimatedDurationMillis
                        (shared.motivationData
                            |> Maybe.map MotivationData.meanRetentionTimes
                            |> Maybe.withDefault []
                        )
                        shared.session
                        |> Millis.toInt
                    )
                |> Time.millisToPosix

        hour =
            String.fromInt <| Time.toHour shared.zone estimate

        minute =
            Time.toMinute shared.zone estimate
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    text <| hour ++ ":" ++ minute
