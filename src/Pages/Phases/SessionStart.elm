module Pages.Phases.SessionStart exposing (Model, Msg, page)

import Delay
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


type Breath
    = In
    | Out


type alias Model =
    { breathingPreview : Breath
    , reminderShown : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { breathingPreview = In
      , reminderShown = False
      }
    , Effect.sendCmd <| Delay.after 2000 ReminderDelayOver
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | ReminderDelayOver


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

        ReminderDelayOver ->
            ( { model | reminderShown = True }
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
        [ BG.color <| rgb255 50 49 46
        , Font.color <| rgb255 200 196 183
        ]
    , element =
        column [ width fill, spacing 100 ]
            [ el [ centerX, centerY ] <|
                if model.reminderShown then
                    viewReminder shared FeatherIcons.volume2

                else
                    none

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
            , el [ centerX, centerY ] <|
                if model.reminderShown then
                    viewReminder shared FeatherIcons.bellOff

                else
                    none
            ]
    }


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
