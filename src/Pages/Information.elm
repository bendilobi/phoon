module Pages.Information exposing (Model, Msg, page)

import Components.Button
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Page exposing (Page)
import Route exposing (Route)
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
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav {}



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ReloadApp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReloadApp ->
            ( model
            , Effect.reloadApp
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Information"
    , attributes =
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb 1 1 1
        ]
    , element =
        textColumn
            [ width fill
            , spacing 50
            , paddingXY 30 50
            ]
            [ paragraph [ Font.size 30, Font.bold ] [ text "Zoff - Wim Hoff Atmung mit dem Hauch von Zen" ]

            -- TODO: Version im service-worker setzen und irgendwie per Javascript über Flags hierher bringen
            , text "Version 0.3.2 \"Der Motivator\""
            , Components.Button.new { onPress = Just ReloadApp, label = text "Reload" }
                |> Components.Button.view
            ]
    }
