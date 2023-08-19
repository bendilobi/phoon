module Pages.Phases.Breathing exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
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


type Breathing
    = AtBreath Int Breath
    | BreathingFinished


type alias Model =
    { breathing : Breathing
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { breathing = AtBreath 1 In }
    , Effect.playSound
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | StartPressed


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                ( newBreathingState, effect ) =
                    case model.breathing of
                        AtBreath n In ->
                            ( AtBreath n Out, Effect.none )

                        AtBreath n Out ->
                            if n < 40 then
                                ( AtBreath (n + 1) In, Effect.none )

                            else
                                ( BreathingFinished, Effect.playSound )

                        BreathingFinished ->
                            ( BreathingFinished, Effect.none )
            in
            ( { model | breathing = newBreathingState }, effect )

        StartPressed ->
            ( { model | breathing = AtBreath 1 In }, Effect.playSound )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.breathing of
        BreathingFinished ->
            Sub.none

        _ ->
            -- WHM App speeds:
            -- Normal 1750
            -- Slow 2500
            -- Quick 1375
            Time.every 1750 Tick



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
                ++ (case model.breathing of
                        AtBreath _ In ->
                            [ BG.color <| rgb255 200 196 183
                            , Font.color <| rgb255 50 49 46
                            ]

                        _ ->
                            []
                   )
            )
        <|
            case model.breathing of
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
                        ]
                    <|
                        text "Retention vorbereiten"
    }
