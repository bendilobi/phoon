module Pages.Phases.SessionStart exposing (Model, Msg, page)

import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
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


type Breath
    = In
    | Out


type alias Model =
    { breathingPreview : Breath
    , ticks : Int
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { breathingPreview = In
      , ticks = 0
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model
                | breathingPreview =
                    case model.breathingPreview of
                        In ->
                            Out

                        Out ->
                            In
                , ticks = model.ticks + 1
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Time.every (Session.speedMillis shared.session |> toFloat) Tick



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Preparation Phase"
    , attributes =
        CS.phaseSessionStart shared.colorScheme
    , element =
        column [ width fill, spacing 100 ]
            [ el
                [ centerX
                , inFront <|
                    row [ spacing 100, centerX ]
                        [ el
                            [ centerX
                            , centerY
                            , transparent <| model.ticks < 2
                            ]
                          <|
                            viewReminder shared FeatherIcons.volume2
                        , el
                            [ centerX
                            , centerY
                            , transparent <| model.ticks < 4
                            ]
                          <|
                            viewReminder shared FeatherIcons.bellOff
                        ]
                ]
                none

            -- TODO: Das synchronisieren mit der Darstellung bei Breathing
            --       => Abwarten, bis die Visualisierung optimiert wird...
            , el
                ([ Font.bold
                 , Font.size 40
                 , width <| px 200
                 , height <| px 200
                 , Border.rounded 100
                 , centerX
                 ]
                    ++ (case model.breathingPreview of
                            In ->
                                CS.sessionStartInverted shared.colorScheme

                            Out ->
                                []
                       )
                )
              <|
                el [ centerX, centerY ] <|
                    text "Start"
            , viewHints model
            ]
    }


viewHints : Model -> Element msg
viewHints model =
    let
        bullet : String -> Element msg
        bullet content =
            row [ spacing 8 ]
                [ el [ alignTop, Font.bold ] <| text "•"
                , paragraph [] [ text content ]
                ]
    in
    el
        [ centerX
        , moveUp 50
        , inFront <|
            textColumn
                [ spacing 20
                , centerX
                , paddingEach { left = 100, right = 100, top = 0, bottom = 0 }
                , Font.size 15
                , transparent <| model.ticks < 8
                ]
                [ bullet "Tippe mit zwei Fingern, um jeweils zur nächsten Übungsphase zu gehen"
                , bullet "Wische mit einem Finger von links nach rechts, um Optionen anzuzeigen"
                , bullet "Teste hier den Sound durch Tipp mit einem Finger"
                ]
        ]
        none


viewReminder : Shared.Model -> FeatherIcons.Icon -> Element msg
viewReminder shared icon =
    if shared.previousPath == Route.Path.PrepareSession then
        row
            [ spacing 10
            , Font.size 30
            ]
            [ el [] <|
                html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 30 icon
            , text "?"
            ]

    else
        none
