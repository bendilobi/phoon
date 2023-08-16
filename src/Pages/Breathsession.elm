module Pages.Breathsession exposing (Model, Msg, page)

-- import Html.Events.Extra.Touch as Etouch

import Dict
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
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
-- type Touched
--     = Untouched
--     | Touched


type alias Model =
    { touchModel : Touch.Model Msg
    , x : Float
    , y : Float
    , touched : Bool
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
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = TouchMsg Touch.Msg
    | MovedOneFinger Float Float
    | MovedTwoFingers Float Float



-- | TouchEnd Etouch.Event


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
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
            , Effect.pushRoute { path = Route.Path.BreathsessionNext, query = Dict.empty, hash = Nothing }
            )

        MovedOneFinger x y ->
            let
                newX =
                    model.x + x

                newY =
                    model.y + y
            in
            ( { model | x = newX, y = newY }
            , if newX > 300 then
                Effect.pushRoute { path = Route.Path.Home_, query = Dict.empty, hash = Nothing }

              else if newY > 200 then
                -- auch hier spielt unter iOS kein Sound... Warum?
                Effect.playSound

              else
                Effect.none
            )



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
                -- el
                --     [ width fill
                --     , height fill
                --     , htmlAttribute <| Etouch.onEnd TouchEnd
                --     ]
                -- <|
                --     none
                html
                <|
                    Touch.element
                        [ HtmlA.style "height" "100%"
                        , HtmlA.style "width" "100%"

                        -- , Etouch.onEnd TouchEnd
                        ]
                        TouchMsg
            ]
            [ column [ centerX, centerY ]
                [ el [] <| text <| "x: " ++ String.fromFloat model.x
                , el [] <| text <| "y: " ++ String.fromFloat model.y
                ]
            ]

    -- [ link
    --     [ centerX
    --     , centerY
    --     ]
    --     { url = "/"
    --     , label = text "Zurück zu Home"
    --     }
    -- ]
    }
