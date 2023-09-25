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
    , soundReminderShown : Bool
    , notificationReminderShown : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { breathingPreview = In
      , soundReminderShown = False
      , notificationReminderShown = False
      }
    , Effect.sendCmd <| Delay.after 1000 SoundReminderDelayOver
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | SoundReminderDelayOver
    | NotificationReminderDelayOver


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
              }
            , Effect.none
            )

        SoundReminderDelayOver ->
            ( { model | soundReminderShown = True }
            , Effect.sendCmd <| Delay.after 1000 NotificationReminderDelayOver
            )

        NotificationReminderDelayOver ->
            ( { model | notificationReminderShown = True }
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
                , centerY
                , transparent <| not model.soundReminderShown
                ]
              <|
                viewReminder shared FeatherIcons.volume2

            -- TODO: Das synchronisieren mit der Darstellung bei Breathing
            --       => Abwarten, bis die Visualisierung optimiert wird...
            , el
                ([ Font.bold
                 , Font.size 40
                 , width <| px 200
                 , height <| px 200
                 , Border.rounded 100
                 ]
                    ++ (case model.breathingPreview of
                            In ->
                                -- [ BG.color <| rgb255 200 196 183
                                -- , Font.color <| rgb255 105 56 112 --50 49 46
                                -- ]
                                CS.sessionStartInverted shared.colorScheme

                            Out ->
                                []
                       )
                )
              <|
                el [ centerX, centerY ] <|
                    text "Start"
            , el
                [ centerX
                , centerY
                , transparent <| not model.notificationReminderShown
                ]
              <|
                viewReminder shared FeatherIcons.bellOff
            ]
    }


viewReminder : Shared.Model -> FeatherIcons.Icon -> Element msg
viewReminder shared icon =
    if shared.previousPath == Route.Path.PrepareSession then
        row
            [ spacing 10
            , Font.size 30

            -- , Font.color <| rgb255 200 196 183
            ]
            [ el [] <|
                html <|
                    FeatherIcons.toHtml [] <|
                        FeatherIcons.withSize 30 icon
            , text "?"
            ]

    else
        none
