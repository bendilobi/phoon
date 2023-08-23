module Pages.Phases.Breathing exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.BreathingSession as BreathingSession
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
        , view = view
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
                            ( AtBreath n Out, Effect.none )

                        AtBreath n Out ->
                            if n < BreathingSession.breathCount shared.session then
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
            Time.every (BreathingSession.speedMillis shared.session |> toFloat) Tick

        _ ->
            Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Atem-Phase"
    , attributes =
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb255 200 196 183
        ]
    , element =
        el
            ([ Font.bold
             , width <| px 300
             , height <| px 300
             , Border.rounded 150
             ]
                ++ (case model of
                        AtBreath _ In ->
                            [ BG.color <| rgb255 200 196 183
                            , Font.color <| rgb255 50 49 46
                            ]

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
