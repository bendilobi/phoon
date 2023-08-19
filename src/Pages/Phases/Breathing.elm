module Pages.Phases.Breathing exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
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
        {}


type Breathing
    = AtBreath Int
    | BreathingFinished


type alias Model =
    { breathing : Breathing
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { breathing = AtBreath 1 }
    , Effect.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                ( newBreathingState, effect ) =
                    case model.breathing of
                        AtBreath n ->
                            if n < 15 then
                                ( AtBreath <| n + 1, Effect.none )

                            else
                                ( BreathingFinished, Effect.playSound )

                        BreathingFinished ->
                            ( BreathingFinished, Effect.none )
            in
            ( { model | breathing = newBreathingState }, effect )



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
    , attributes =
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb255 255 255 255
        ]
    , element =
        el
            [ Font.bold
            , Font.size 40
            ]
        <|
            case model.breathing of
                AtBreath n ->
                    text <| String.fromInt n

                BreathingFinished ->
                    text <| "Done!"
    }
