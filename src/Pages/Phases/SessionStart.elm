module Pages.Phases.SessionStart exposing (Model, Msg, page)

-- import Lib.Utils exposing (bullet)

import Components.AnimatedButton as Button
import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Delay
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Html.Attributes
import Layouts
import Layouts.BaseLayout
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.PageFading as Fading exposing (Trigger(..))
import Lib.Session as Session
import Lib.SessionResults as SessionResults
import Lib.Utils exposing (bullet)
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
        , update = update shared
        , subscriptions = subscriptions shared
        , view = view shared
        }
        |> Page.withLayout (toLayout shared)


toLayout : Shared.Model -> Model -> Layouts.Layout Msg
toLayout shared model =
    Layouts.BaseLayout_SessionControls
        { currentCycle = SessionResults.finishedCycles shared.results
        , controlsTop = []
        , controlsBottom = [ viewCancelButton shared model ]
        , fadeOut = model.fadeOut
        , overlay = Layouts.BaseLayout.NoOverlay
        , multitouchEffects = [ Effect.navigateNext shared.session ]
        , singleTapEffects = [ Effect.playSound Session.StartSound ]
        }



-- INIT


type alias Model =
    { bubble : Bubble.Model Msg
    , ticks : Int
    , cancelButton : Button.Model
    , fadeOut : Fading.Trigger
    , fadeInFinished : Bool
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
      , cancelButton = Button.init
      , fadeOut = NoFade
      , fadeInFinished = False
      }
    , Effect.sendCmd <| Delay.after (Fading.duration + 500) FadeInFinished
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | OnCancelButton Button.Model
    | FadeOutFinished
    | FadeInFinished


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
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

        OnCancelButton newState ->
            ( { model
                | cancelButton = newState
                , fadeOut =
                    if shared.previousPath == Session.phasePath Session.End then
                        NoFade

                    else if newState == Button.Triggered then
                        FadeWith Fading.sessionFadingColor

                    else
                        NoFade
              }
            , if newState == Button.Triggered then
                if shared.previousPath == Session.phasePath Session.End then
                    Effect.cancelSession shared.session

                else
                    Effect.sendCmd <| Delay.after Fading.duration FadeOutFinished

              else
                Effect.none
            )

        FadeInFinished ->
            ( { model | fadeInFinished = True }, Effect.none )

        FadeOutFinished ->
            ( model
            , Effect.navigate (FadeWith Fading.sessionFadingColor) shared.previousPath
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    if model.fadeInFinished then
        --- Reducing the tickSpeed by 1 millisecond is a fix for a bug in Elm:
        --- https://github.com/elm/time/issues/25
        Time.every (Bubble.tickSpeed model.bubble - 1) Tick

    else
        Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Preparation Phase"
    , attributes =
        CS.phaseSessionStart shared.colorScheme
    , element =
        let
            container =
                if shared.deviceInfo.orientation == Portrait then
                    column

                else
                    row
        in
        container
            [ width fill
            , height fill
            , spacing 100
            ]
            [ el [ height fill, width fill ] none
            , el
                [ centerX
                , centerY
                , transparent <| model.ticks < 1
                ]
              <|
                viewReminder shared FeatherIcons.volume2
            , el [ width fill, height fill ] <|
                if model.fadeInFinished then
                    el [ centerX, centerY ] <|
                        (Bubble.new
                            { model = model.bubble
                            , size =
                                min shared.deviceInfo.window.width
                                    shared.deviceInfo.window.height
                                    * 0.5
                                    |> round
                            , bubbleColor = CS.phaseSessionStartCopyColor shared.colorScheme
                            , bgColor = CS.phaseSessionStartColor shared.colorScheme
                            }
                            |> Bubble.withLabel "Start"
                            |> Bubble.withFontSize 50
                            |> Bubble.view
                        )

                else
                    none
            , el
                [ width fill
                , inFront <| viewHints shared
                ]
              <|
                el
                    [ centerX
                    , centerY
                    , transparent <| model.ticks < 2
                    ]
                <|
                    viewReminder shared FeatherIcons.bellOff
            , el
                [ width fill
                , height fill
                ]
                none

            -- , viewHints shared
            ]
    }


viewHints : Shared.Model -> Element msg
viewHints shared =
    -- let
    --     bullet : String -> Element msg
    --     bullet content =
    --         row [ spacing 8 ]
    --             [ el [ alignTop, Font.bold ] <| text "•"
    --             , paragraph
    --                 [ --- This is a bugfix for (it seems) a bug in elm-ui...
    --                   --- See https://github.com/mdgriffith/elm-ui/issues/124
    --                   --- Without this, the button that is overlayed on swipe in the
    --                   --- SessionControls is not clickable at first, only on the second
    --                   --- tap...
    --                   htmlAttribute <| Html.Attributes.style "pointer-events" "none"
    --                 ]
    --                 [ text content ]
    --             ]
    -- in
    column
        [ spacing 20

        -- , paddingEach { left = 70, right = 70, top = 0, bottom = 0 }
        , paddingXY 30 0
        , Font.size 15
        , moveUp 50
        , transparent True
        ]
        -- [ bullet "Tippe mit drei Fingern, um mit der Übung zu beginnen"
        -- , bullet "Wische mit einem Finger, um Optionen anzuzeigen"
        [ bullet <| text "Tippe mit drei Fingern, um mit der Übung zu beginnen"
        , bullet <| text "Wische mit einem Finger, um Optionen anzuzeigen"

        -- , bullet "Teste hier den Sound durch Tipp mit einem Finger"
        ]



-- [ text "Tippe mit drei Fingern, um jeweils zur nächsten Übungsphase zu gehen"
-- , text "Wische mit einem Finger von links nach rechts, um Optionen anzuzeigen"
-- , text "Teste hier den Sound durch Tipp mit einem Finger"
-- ]


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


viewCancelButton : Shared.Model -> Model -> Element Msg
viewCancelButton shared model =
    --TODO: Texte zwischen den verschiedenen CancelButtons auf den Phasen-Seiten synchronisieren
    Button.new
        { model = model.cancelButton
        , label = text "Sitzung abbrechen"
        , onPress = OnCancelButton
        }
        |> Button.view shared.colorScheme
