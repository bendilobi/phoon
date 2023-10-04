module Pages.Phases.Breathing exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Session as Session
import Lib.Utils as Utils
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update shared
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.SessionControls
        { showSessionProgress = True }


type Breath
    = In
    | Out


type Model
    = Starting
    | AtBreath Int Breath
    | BreathingFinished


init : () -> ( Model, Effect Msg )
init () =
    ( Starting
    , Effect.batch
        [ Effect.playSound Utils.Breathing
        , Effect.sendMsg <| Tick <| Time.millisToPosix 0
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            let
                ( newBreathingState, effect ) =
                    case model of
                        Starting ->
                            ( AtBreath 1 In, Effect.none )

                        AtBreath n In ->
                            if n == Session.breathCount shared.session then
                                ( BreathingFinished, Effect.playSound Utils.Breathing )

                            else
                                ( AtBreath n Out, Effect.none )

                        AtBreath n Out ->
                            if n < Session.breathCount shared.session then
                                ( AtBreath (n + 1) In, Effect.none )

                            else
                                ( BreathingFinished, Effect.playSound Utils.Breathing )

                        BreathingFinished ->
                            ( BreathingFinished, Effect.none )
            in
            ( newBreathingState, effect )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    case model of
        AtBreath _ _ ->
            Time.every (Session.speedMillis shared.session |> toFloat) Tick

        _ ->
            Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Atem-Phase"
    , attributes =
        CS.phaseBreathing shared.colorScheme
    , element =
        el [ width fill, height fill ] <|
            el
                ([ Font.bold
                 , width <| px 300
                 , height <| px 300
                 , Border.rounded 150
                 , centerX
                 , centerY
                 ]
                    ++ (case model of
                            AtBreath _ In ->
                                CS.breathingInverted shared.colorScheme

                            _ ->
                                []
                       )
                )
            <|
                case model of
                    Starting ->
                        none

                    AtBreath n _ ->
                        el
                            [ centerX
                            , centerY
                            , Font.size 120
                            ]
                        <|
                            text <|
                                String.fromInt n

                    BreathingFinished ->
                        el
                            [ centerX
                            , centerY
                            , Font.size 40
                            , Font.center
                            ]
                        <|
                            text "Retention \nvorbereiten"
    }
