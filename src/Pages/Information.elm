module Pages.Information exposing (Model, Msg, page)

import Browser.Navigation
import Components.Button
import Date
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Layouts
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.MotivationData as MotivationData exposing (MotivationData)
import Page exposing (Page)
import Route exposing (Route)
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
        |> Page.withLayout toLayout


toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.MainNav {}



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
    = ReloadApp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReloadApp ->
            ( model
            , Effect.sendCmd Browser.Navigation.reload
              --AndSkipCache
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Information"
    , attributes =
        CS.primaryInformation shared.colorScheme
    , element =
        column
            [ width fill
            , spacing 30
            , paddingXY 20 30
            , Font.size 15
            ]
            [ viewIntroduction shared
            , viewSettings shared
            , viewTechInfo shared
            ]
    }


viewIntroduction : Shared.Model -> Element Msg
viewIntroduction shared =
    column [ width fill, spacing 10 ]
        [ paragraph
            [ Font.size 25
            , Font.bold
            , Font.color <| CS.guideColor shared.colorScheme
            ]
            [ text "Zoff - Wim Hof Atmung mit dem Hauch von Zen" ]

        -- TODO: Version im service-worker setzen und irgendwie per Javascript über Flags hierher bringen
        , paragraph [] [ text """
        Mit Zoff machst Du Deine Atemübung ganz entspannt, vielleicht sogar im Liegen und mit geschlossenen
        Augen - Klänge leiten Dich jeweils zum nächsten Schritt. Und wenn Du selbst entscheiden möchtest, wann es 
        weitergeht (z.B. Beginn und Ende der Retention), tippst Du einfach mit zwei Fingern irgendwo auf den Bildschirm.
        """ ]
        , row [ width fill, paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ]
            [ el [ Font.size 13, alignBottom ] <| text "Version 0.4.12 \"Sunrise\""
            , el [ width fill ] <|
                el [ alignRight ] <|
                    (Components.Button.new { onPress = Just ReloadApp, label = text "App neu laden" }
                        |> Components.Button.withInline True
                        |> Components.Button.view shared.colorScheme
                    )
            ]
        ]


viewSettings : Shared.Model -> Element Msg
viewSettings shared =
    text "TODO: Settings"


viewTechInfo : Shared.Model -> Element msg
viewTechInfo shared =
    el [] <|
        case MotivationData.getMotivationData shared.motivationData of
            Nothing ->
                text "Noch keine Motivationsdaten vorhanden"

            Just data ->
                column [ spacing 10 ]
                    [ el
                        [ Font.bold
                        , Font.size 17
                        , Font.color <| CS.guideColor shared.colorScheme
                        ]
                      <|
                        text "Aktuell gespeicherte Motivationsdaten"
                    , text <| "Serie: " ++ String.fromInt data.series
                    , text <| "Letzte Sitzung: " ++ Date.toIsoString data.lastSessionDate
                    , text <| "Mittlere Ret: " ++ (String.join "," <| List.map String.fromInt data.meanRetentiontimes)
                    , text <| "Max Ret: " ++ String.fromInt data.maxRetention
                    ]
