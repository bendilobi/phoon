module Layouts.SessionControls exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Layout exposing (Layout)
import Lib.BreathingSession as BS
import Lib.Swipe as Swipe
import Lib.Tools as Tools
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update shared route
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { gesture : Swipe.Gesture
    , controlsShown : Bool
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { gesture = Swipe.blanco
      , controlsShown = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = Swipe Swipe.Event
    | SwipeEnd Swipe.Event
    | Cancelled


update : Shared.Model -> Route () -> Msg -> Model -> ( Model, Effect Msg )
update shared route msg model =
    case msg of
        Swipe touch ->
            ( { model | gesture = Swipe.record touch model.gesture }
            , Effect.none
            )

        SwipeEnd touch ->
            let
                gesture =
                    Swipe.record touch model.gesture

                session =
                    if Swipe.isTap gesture then
                        BS.goNext shared.session

                    else
                        shared.session

                currentPath =
                    BS.currentPath session
            in
            ( { model
                | gesture = Swipe.blanco
                , controlsShown = Swipe.isRightSwipe 300 gesture
              }
            , Effect.batch
                [ Effect.sessionUpdated session
                , if route.path == currentPath then
                    Effect.none

                  else
                    Tools.navigate <| currentPath
                ]
            )

        Cancelled ->
            ( model, Tools.navigate Route.Path.Home_ )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view { toContentMsg, model, content } =
    { title = content.title ++ " | Zoff"
    , attributes = []
    , element =
        column
            [ width fill
            , height fill
            , BG.color <| rgb255 38 86 86
            , Font.color <| rgb255 255 255 255
            ]
            [ el
                [ width fill
                , height fill
                , inFront (viewTouchOverlay |> map toContentMsg)
                ]
              <|
                el [ centerX, centerY ] <|
                    content.element
            , if model.controlsShown then
                viewSessionControls model |> map toContentMsg

              else
                none
            ]
    }


viewTouchOverlay : Element Msg
viewTouchOverlay =
    el
        [ width fill
        , height fill
        , htmlAttribute <| Swipe.onStart Swipe
        , htmlAttribute <| Swipe.onMove Swipe
        , htmlAttribute <| Swipe.onEnd SwipeEnd
        ]
        none


viewSessionControls : Model -> Element Msg
viewSessionControls model =
    column [ centerX, centerY ]
        [ button
            [ height <| px 100
            , paddingXY 10 200
            , BG.color <| rgb255 50 49 46
            ]
            { onPress = Just Cancelled
            , label = text "Sitzung abbrechen"
            }
        ]



-- , body =
--     [ Html.text "SessionControls"
--     , Html.div [ class "page" ] content.body
--     ]
-- }
