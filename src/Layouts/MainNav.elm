module Layouts.MainNav exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Layout exposing (Layout)
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
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = MotivationClicked
    | SessionClicked
    | InformationClicked


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        MotivationClicked ->
            ( model, Effect.navigate Route.Path.Home_ )

        SessionClicked ->
            ( model, Effect.navigate Route.Path.Session )

        InformationClicked ->
            ( model, Effect.navigate Route.Path.Information )


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
            (content.attributes
                ++ [ width fill
                   , height fill
                   ]
            )
            [ el [ height fill, width fill ] content.element
            , viewNavBar |> map toContentMsg
            ]
    }


viewNavBar : Element Msg
viewNavBar =
    row
        [ width fill
        , height <| px 70
        , BG.color <| rgb 1 1 1
        , Font.color <| rgb 0 0 0
        , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
        , Border.color <| rgb255 200 196 166

        -- TODO: Gibt es einen Standard-Abstand oder kann man die Safe Area irgendwie ermitteln?
        , paddingEach { bottom = 15, top = 0, left = 0, right = 0 }
        ]
        [ button
            [ alignLeft
            , paddingXY 20 0
            , height fill
            ]
            { onPress = Just MotivationClicked
            , label = text "Moti"
            }
        , button
            [ centerX
            , paddingXY 20 0
            , height fill
            ]
            { onPress = Just SessionClicked
            , label = text "Go"
            }
        , button
            [ alignRight
            , paddingXY 20 0
            , height fill
            ]
            { onPress = Just InformationClicked
            , label = text "Info"
            }
        ]
