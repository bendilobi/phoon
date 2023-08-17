module Pages.Breathsession exposing (Model, Msg, page)

-- import Html.Events.Extra.Touch as Etouch

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Element.Keyed as Keyed
import Html
import Html.Attributes as HtmlA
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Time
import Touch
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT
-- type Touched
--     = Untouched
--     | Touched


type
    Breathing
    -- TODO: ReadyToStart rausnehmen, wenn ichs eh nicht verwende
    = ReadyToStart
    | AtBreath Int
    | BreathingFinished
    | Paused


type alias Model =
    { touchModel : Touch.Model Msg
    , x : Float
    , y : Float
    , touched : Bool
    , breathing : Breathing
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { touchModel =
            Touch.initModel
                [ Touch.onMove { fingers = 1 } MovedOneFinger
                , Touch.onMove { fingers = 2 } MovedTwoFingers
                ]
      , x = 0
      , y = 0
      , touched = False
      , breathing = AtBreath 0
      }
    , Effect.setWakeLock
    )



-- UPDATE


type Msg
    = TouchMsg Touch.Msg
    | MovedOneFinger Float Float
    | MovedTwoFingers Float Float
    | Tick Time.Posix
    | StartBreathing
    | Cancelled



-- | TouchEnd Etouch.Event


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
                            if n < 5 then
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

        TouchMsg touchMsg ->
            -- let
            --     m =
            --         { model
            --             | touched =
            --                 if List.length touchMsg.touches >= 1 then
            --                     not model.touched
            --                 else
            --                     model.touched
            --         }
            -- in
            Touch.update touchMsg model.touchModel (\newTouchModel -> { model | touchModel = newTouchModel })
                |> (\( mdl, cmdMsg ) -> ( mdl, Effect.sendCmd cmdMsg ))

        MovedTwoFingers x y ->
            ( { model | touched = not model.touched }
            , Effect.replaceRoute { path = Route.Path.BreathsessionNext, query = Dict.empty, hash = Nothing }
            )

        MovedOneFinger x y ->
            let
                newX =
                    model.x + x

                newY =
                    model.y + y
            in
            ( { model
                | x = newX
                , y = newY
                , breathing =
                    if model.x > 300 then
                        Paused

                    else
                        model.breathing
              }
            , Effect.none
            )

        Cancelled ->
            ( model, Effect.replaceRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing } )



-- TouchEnd event ->
--     ( { model
--         | touched =
--             --not model.touched
--             if List.length event.touches >= 1 then
--                 not model.touched
--             else
--                 model.touched
--       }
--     , Effect.none
--     )
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
    { title = "Zoff - Session"
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
            , inFront <|
                case model.breathing of
                    ReadyToStart ->
                        none

                    Paused ->
                        none

                    _ ->
                        -- el
                        --     [ width fill
                        --     , height fill
                        --     , htmlAttribute <| Etouch.onEnd TouchEnd
                        --     ]
                        -- <|
                        --     none
                        html <|
                            Touch.element
                                [ HtmlA.style "height" "100%"
                                , HtmlA.style "width" "100%"

                                -- , Etouch.onEnd TouchEnd
                                ]
                                TouchMsg
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

                -- String.fromInt model.breathsDone
                , el [ centerX ] <| text <| "x: " ++ String.fromFloat model.x
                , el [ centerX ] <| text <| "y: " ++ String.fromFloat model.y

                -- ### Das hier funktioniert wunderbar im Chrome, aber iOS Safari spielt nur den ersten Sound, nicht bei BreathingFinished...
                -- , Keyed.el [] <|
                --     ( case model.breathing of
                --         ReadyToStart ->
                --             "ready"
                --         AtBreath _ ->
                --             "breathing"
                --         BreathingFinished ->
                --             "done"
                --     , html <|
                --         Html.audio
                --             [ HtmlA.src "/audio/bell.mp3"
                --             , HtmlA.id "audioplayer"
                --             , HtmlA.controls False
                --             , HtmlA.autoplay <|
                --                 case model.breathing of
                --                     ReadyToStart ->
                --                         False
                --                     AtBreath _ ->
                --                         True
                --                     BreathingFinished ->
                --                         True
                --             ]
                --             []
                --     )
                ]
            ]
    }
