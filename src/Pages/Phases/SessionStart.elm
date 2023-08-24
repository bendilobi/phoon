module Pages.Phases.SessionStart exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Layouts
import Lib.Session as Session
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Time
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions shared
        , view = view shared
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


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Preparation Phase"
    , attributes =
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb255 200 196 183
        ]
    , element =
        column [ width fill, spacing 60 ]
            -- [ row [ centerX, spacing 20 ]
            -- [ viewIcon FeatherIcons.volumeX
            -- , viewIcon FeatherIcons.arrowRight
            -- , viewIcon FeatherIcons.volume2
            -- ]
            [ el [ centerX, centerY ] <| viewReminder shared FeatherIcons.volume2

            -- TODO: Das synchronisieren mit der Darstellung bei Breathing
            , el
                ([ Font.bold
                 , Font.size 40
                 , width <| px 200
                 , height <| px 200
                 , Border.rounded 100
                 ]
                    ++ (case model of
                            In ->
                                [ BG.color <| rgb255 200 196 183
                                , Font.color <| rgb255 50 49 46
                                ]

                            Out ->
                                []
                       )
                )
              <|
                el [ centerX, centerY ] <|
                    text "Start"

            -- , row [ centerX, spacing 20 ]
            -- [ viewIcon FeatherIcons.bell
            -- , viewIcon FeatherIcons.arrowRight
            -- , viewIcon FeatherIcons.bellOff
            -- ]
            , el [ centerX, centerY ] <| viewReminder shared FeatherIcons.bellOff
            ]
    }



-- viewIcon : FeatherIcons.Icon -> Element msg
-- viewIcon icon =
--     el [ Font.color <| rgb255 200 196 183 ] <|
--         html <|
--             FeatherIcons.toHtml [] <|
--                 FeatherIcons.withSize 30 icon


viewReminder : Shared.Model -> FeatherIcons.Icon -> Element msg
viewReminder shared icon =
    if shared.previousPath == Route.Path.PrepareSession then
        row
            [ spacing 10
            , Font.size 30
            , Font.color <| rgb255 200 196 183
            ]
            [ el [] <|
                html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 30 icon
            , text "?"
            ]

    else
        none
