module Pages.Phases.SessionStart exposing (Model, Msg, page)

import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Html.Attributes
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
        { init = init shared
        , update = update
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.SessionControls
        { showCurrentCycle = Nothing
        , ifCancelled =
            if not (shared.previousPath == Session.phasePath Session.End) then
                Effect.navigate shared.previousPath

            else
                Effect.cancelSession
        }



-- INIT


type alias Model =
    { bubble : Bubble.Model Msg
    , ticks : Int
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { ticks = 0
      , bubble =
            Bubble.init
                { bubbleType = Bubble.Static
                , onFinished = Nothing
                , breathingSpeed = Session.speedMillis shared.session
                }
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
            let
                ( bModel, _ ) =
                    Bubble.update
                        { msg = Bubble.Tick
                        , model = model.bubble
                        , toModel = \bubble -> { model | bubble = bubble }
                        }
            in
            ( { model
                | ticks = model.ticks + 1
                , bubble = bModel.bubble
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    --- Reducing the tickSpeed by 1 millisecond is a fix for a bug in Elm:
    --- https://github.com/elm/time/issues/25
    Time.every (Bubble.tickSpeed model.bubble - 1) Tick



-- Sub.none
-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Preparation Phase"
    , attributes =
        CS.phaseSessionStart shared.colorScheme
    , element =
        column [ width fill, height fill, spacing 100 ]
            [ row
                [ spacing 100
                , centerX
                , paddingEach { top = 150, bottom = 0, left = 0, right = 0 }
                ]
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
            , el [ width fill, height fill ] <|
                el [ centerX ] <|
                    (Bubble.new
                        { model = model.bubble

                        -- , size = 200
                        , size =
                            min shared.deviceInfo.window.width
                                shared.deviceInfo.window.height
                                * 0.4
                                |> round
                        , bubbleColor = CS.phaseSessionStartCopyColor shared.colorScheme
                        , bgColor = CS.phaseSessionStartColor shared.colorScheme
                        }
                        |> Bubble.withLabel "Start"
                        |> Bubble.withFontSize 50
                        |> Bubble.view
                    )
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
                , paragraph
                    [ --- This is a bugfix for (it seems) a bug in elm-ui...
                      --- See https://github.com/mdgriffith/elm-ui/issues/124
                      --- Without this, the button that is overlayed on swipe in the
                      --- SessionControls is not clickable at first, only on the second
                      --- tap...
                      htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                    ]
                    [ text content ]
                ]
    in
    column
        [ spacing 20
        , paddingEach { left = 70, right = 70, top = 0, bottom = 100 }
        , Font.size 15
        , transparent <| model.ticks < 8
        ]
        [ bullet "Tippe mit zwei Fingern, um jeweils zur nächsten Übungsphase zu gehen"
        , bullet "Wische mit einem Finger von links nach rechts, um Optionen anzuzeigen"
        , bullet "Teste hier den Sound durch Tipp mit einem Finger"
        ]


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
