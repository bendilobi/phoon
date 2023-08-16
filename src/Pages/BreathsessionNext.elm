module Pages.BreathsessionNext exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Html
import Html.Attributes as HtmlA
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
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


type alias Model =
    { touchModel : Touch.Model Msg
    , x : Float
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { touchModel =
            Touch.initModel
                [ Touch.onMove { fingers = 1 } MovedOneFinger ]
      , x = 0
      }
    , Effect.playSound
    )



-- UPDATE


type Msg
    = TouchMsg Touch.Msg
    | MovedOneFinger Float Float


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        TouchMsg touchMsg ->
            Touch.update touchMsg model.touchModel (\newTouchModel -> { model | touchModel = newTouchModel })
                |> (\( mdl, cmdMsg ) -> ( mdl, Effect.sendCmd cmdMsg ))

        MovedOneFinger x _ ->
            let
                newX =
                    model.x + x
            in
            ( { model | x = newX }
            , if newX > 300 then
                Effect.replaceRoute { path = Route.Path.PauseSession, query = Dict.empty, hash = Nothing }

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
            , inFront <|
                html <|
                    Touch.element
                        [ HtmlA.style "height" "100%"
                        , HtmlA.style "width" "100%"
                        ]
                        TouchMsg
            ]
            [ column [ centerX, centerY ]
                [ el [] <| text <| "2. Phase..."
                ]
            ]
    }
