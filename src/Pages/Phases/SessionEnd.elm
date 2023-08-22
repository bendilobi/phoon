module Pages.Phases.SessionEnd exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Lib.SessionResults as SessionResults
import Lib.Utils as Utils
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
        , view = view shared
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.playSound Utils.SessionEnd
    )



-- UPDATE


type Msg
    = NavigateHome


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NavigateHome ->
            ( model
            , Effect.navigate Route.Path.Home_
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
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
            , viewRetentionTimes <| SessionResults.getRetentionTimes shared.results
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


viewRetentionTimes : List Int -> Element msg
viewRetentionTimes times =
    let
        meanTime =
            List.sum times // List.length times
    in
    column
        [ spacing 10
        , centerX
        , centerY
        , Font.alignRight
        ]
    <|
        List.map2
            (\i t ->
                row [ width fill ]
                    [ el [ width fill ] <| text <| "Runde " ++ String.fromInt i ++ ": "
                    , el [ Font.bold ] <| text <| formatRetentionTime t
                    ]
            )
            (List.range 1 (List.length times))
            times
            ++ [ row
                    [ width fill
                    , Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                    , paddingXY 0 7
                    ]
                    [ el [] <| text "Durchschnitt: "
                    , el
                        [ Font.bold
                        ]
                      <|
                        text <|
                            formatRetentionTime meanTime
                    ]
               ]


formatRetentionTime : Int -> String
formatRetentionTime seconds =
    String.join ":"
        [ String.padLeft 1 '0' <| String.fromInt <| remainderBy 60 (seconds // 60)
        , String.padLeft 2 '0' <| String.fromInt <| remainderBy 60 seconds
        ]
