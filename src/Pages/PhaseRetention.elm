module Pages.PhaseRetention exposing (Model, Msg, page)

-- import Html
-- import Html.Attributes as HtmlA
-- import Touch

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Lib.Swipe as Swipe
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
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


type alias Model =
    { gesture : Swipe.Gesture
    , paused : Bool

    -- , touchModel : Touch.Model Msg
    -- , x : Float
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { gesture = Swipe.blanco
      , paused = False

      --   , touchModel =
      --         Touch.initModel
      --             [ Touch.onMove { fingers = 2 } MovedTwoFingers ]
      --   , x = 0
      }
    , Effect.playSound
    )



-- UPDATE


type Msg
    = Swipe Swipe.Event
    | SwipeEnd Swipe.Event



-- | TouchMsg Touch.Msg
-- | MovedTwoFingers Float Float


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        -- TouchMsg touchMsg ->
        --     Touch.update touchMsg model.touchModel (\newTouchModel -> { model | touchModel = newTouchModel })
        --         |> (\( mdl, cmdMsg ) -> ( mdl, Effect.sendCmd cmdMsg ))
        -- MovedTwoFingers _ _ ->
        --     ( model
        --     , Effect.replaceRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing }
        --     )
        Swipe touch ->
            ( { model | gesture = Swipe.record touch model.gesture }
            , Effect.none
            )

        SwipeEnd touch ->
            let
                gesture : Swipe.Gesture
                gesture =
                    Swipe.record touch model.gesture
            in
            ( { model | gesture = Swipe.blanco, paused = Swipe.isRightSwipe 300 gesture }
            , if Swipe.isTap gesture then
                Effect.replaceRoute { path = Route.Path.PhaseRelaxRetention, query = Dict.empty, hash = Nothing }

              else
                Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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
            , Background.color <| rgb255 38 86 86
            , Font.color <| rgb255 255 255 255
            , htmlAttribute <| Swipe.onStart Swipe
            , htmlAttribute <| Swipe.onMove Swipe
            , htmlAttribute <| Swipe.onEnd SwipeEnd

            -- , inFront <|
            --     html <|
            --         Html.div
            --             [ HtmlA.style "height" "100%"
            --             , HtmlA.style "width" "100%"
            --             , Swipe.onStart Swipe
            --             , Swipe.onMove Swipe
            --             , Swipe.onEnd SwipeEnd
            --             ]
            --             []
            -- Touch.element
            --     [ HtmlA.style "height" "100%"
            --     , HtmlA.style "width" "100%"
            --     -- , Swipe.onStart Swipe
            --     -- , Swipe.onMove Swipe
            --     -- , Swipe.onEnd SwipeEnd
            --     ]
            --     TouchMsg
            ]
            [ column [ centerX, centerY ]
                [ el [] <| text "Retention..."
                , if model.paused then
                    el [] <| text "Pausiert"

                  else
                    none
                ]
            ]
    }
