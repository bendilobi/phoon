module Pages.Phases.Breathing exposing (Model, Msg, page)

import Components.BreathingBubble as Bubble exposing (BreathingBubble)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Session as Session
import Lib.Utils as Utils
import Page exposing (Page)
import Route exposing (Route)
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
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.SessionControls
        { showSessionProgress = True }


type alias Model =
    { bubble : Bubble.Model Msg
    , breathingFinished : Bool
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { bubble =
            Bubble.init
                { bubbleType = Bubble.Counting <| Session.breathCount shared.session
                , onFinished = Just BubbleFinished
                }
      , breathingFinished = False
      }
    , Effect.batch
        [ Effect.playSound Utils.Breathing
        ]
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | BubbleFinished


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Tick _ ->
            Bubble.update
                { msg = Bubble.Tick
                , model = model.bubble
                , toModel = \bubble -> { model | bubble = bubble }
                }

        BubbleFinished ->
            ( { model | breathingFinished = True }
            , Effect.playSound Utils.Breathing
            )



-- SUBSCRIPTIONS


subscriptions : Shared.Model -> Model -> Sub Msg
subscriptions shared model =
    Time.every (Session.speedMillis shared.session |> toFloat) Tick



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Atem-Phase"
    , attributes =
        CS.phaseBreathing shared.colorScheme
    , element =
        el [ width fill, height fill ] <|
            if model.breathingFinished then
                el
                    [ centerX
                    , centerY
                    , Font.size 40
                    , Font.center
                    ]
                <|
                    text "Retention \nvorbereiten"

            else
                Bubble.new { model = model.bubble, size = 300 }
                    |> Bubble.view shared.colorScheme
    }
