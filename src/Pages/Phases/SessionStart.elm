module Pages.Phases.SessionStart exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.Session as Session
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
        , subscriptions = subscriptions shared
        , view = view
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.SessionControls
        { showSessionProgress = False }



-- INIT


type Model
    = In
    | Out


init : () -> ( Model, Effect Msg )
init () =
    ( In
    , Effect.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case ( msg, model ) of
        ( Tick _, In ) ->
            ( Out, Effect.none )

        ( Tick _, Out ) ->
            ( In, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Time.every (Session.speedMillis shared.session |> toFloat) Tick



-- VIEW


view : Model -> View Msg
view model =
    { title = "Preparation Phase"
    , attributes =
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb255 200 196 183
        ]
    , element =
        -- TODO: Feather Icon volume-x und Fragezeichen nutzen, als Hinweis auf Stummschaltung aufheben
        -- Oder volume-x, ein Pfeil und volume-2?
        -- Dazu wifiOff mit Fragezeichen?
        el
            ([ Font.bold
             , Font.size 40
             , width <| px 200
             , height <| px 200
             , Border.rounded 100
             ]
                ++ (case model of
                        In ->
                            inBreathAttrs

                        Out ->
                            []
                   )
            )
        <|
            el [ centerX, centerY ] <|
                text "Start"
    }


inBreathAttrs : List (Attribute msg)
inBreathAttrs =
    [ BG.color <| rgb255 200 196 183
    , Font.color <| rgb255 50 49 46
    ]
