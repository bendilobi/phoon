module Pages.SessionEnd exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Element.Input exposing (button)
import Lib.Tools as Tools
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
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NavigateHome


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NavigateHome ->
            ( model
            , Tools.navigate Route.Path.Home_
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Session End"
    , attributes =
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb255 255 255 255
        ]
    , element =
        column
            [ width fill
            , height fill
            , spacing 30
            ]
            [ el
                [ centerX
                , centerY
                , Font.bold
                , Font.size 40
                ]
              <|
                text "Sitzung beendet!"
            , button
                [ centerX
                , centerY
                , BG.color <| rgb255 30 30 30
                , padding 20
                ]
                { onPress = Just NavigateHome
                , label = text "Zurück zum Start"
                }
            ]
    }
