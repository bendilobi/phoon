module Pages.PhaseBreathing exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
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
        {}


type
    Breathing
    -- TODO: ReadyToStart rausnehmen, wenn ichs eh nicht verwende
    = ReadyToStart
    | AtBreath Int
    | BreathingFinished
    | Paused


type alias Model =
    { touched : Bool
    , breathing : Breathing
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { touched = False
      , breathing = AtBreath 0
      }
    , Effect.setWakeLock
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | StartBreathing
    | Cancelled


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                ( newBreathingState, effect ) =
                    case model.breathing of
                        ReadyToStart ->
                            ( ReadyToStart, Effect.none )

                        AtBreath n ->
                            if n < 15 then
                                ( AtBreath <| n + 1, Effect.none )

                            else
                                ( BreathingFinished, Effect.playSound )

                        BreathingFinished ->
                            ( BreathingFinished, Effect.none )

                        Paused ->
                            ( Paused, Effect.none )
            in
            ( { model | breathing = newBreathingState }, effect )

        StartBreathing ->
            ( { model | breathing = AtBreath 0 }, Effect.none )

        Cancelled ->
            ( model, Effect.replaceRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.breathing of
        AtBreath _ ->
            Time.every 1000 Tick

        _ ->
            Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Breathing Phase"
    , attributes = []
    , element =
        column
            [ width fill
            , height fill
            , if not model.touched then
                Background.color <| rgb255 50 49 46

              else
                Background.color <| rgb255 0 0 0
            , Font.color <| rgb255 255 255 255
            ]
            [ column [ centerX, centerY ]
                [ el
                    [ paddingXY 0 10
                    , Font.size 40
                    , Font.bold
                    , centerX
                    ]
                  <|
                    case model.breathing of
                        ReadyToStart ->
                            button []
                                { onPress = Just StartBreathing
                                , label = text "Start"
                                }

                        AtBreath n ->
                            text <| String.fromInt n

                        BreathingFinished ->
                            text <| "Done!"

                        Paused ->
                            button []
                                { onPress = Just Cancelled
                                , label = text "Pause..."
                                }
                ]
            ]
    }
