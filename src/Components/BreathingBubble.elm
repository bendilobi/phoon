module Components.BreathingBubble exposing
    ( BreathingBubble
    , BubbleType(..)
    , Model
    , Msg(..)
    , init
    , new
    , tickSpeed
    , update
    , view
    , withBreathCount
    , withFontSize
    , withLabel
    , withSpeed
    )

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Lib.Millis as Millis exposing (Milliseconds)
import Lib.Utils as Utils
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Property as P



--- Settings ---


type BreathingBubble msg
    = Settings
        { model : Model msg
        , size : Int
        , label : Maybe String
        , fontSize : Maybe Int
        , bubbleColor : Color
        , bgColor : Color
        }


new :
    { model : Model msg
    , size : Int
    , bubbleColor : Color
    , bgColor : Color
    }
    -> BreathingBubble msg
new props =
    Settings
        { model = props.model
        , size = props.size
        , label = Nothing
        , fontSize = Nothing
        , bubbleColor = props.bubbleColor
        , bgColor = props.bgColor
        }



--- Modifiers ---


withLabel : String -> BreathingBubble msg -> BreathingBubble msg
withLabel label (Settings settings) =
    Settings { settings | label = Just label }


withFontSize : Int -> BreathingBubble msg -> BreathingBubble msg
withFontSize size (Settings settings) =
    Settings { settings | fontSize = Just size }



--- Model ---


type BubbleType
    = Static
    | Counting Int


type Model msg
    = Model
        { currentBreath : Int
        , breathingSpeed : Milliseconds
        , bubbleType : BubbleType
        , onFinished : Maybe msg
        }


init :
    { bubbleType : BubbleType
    , onFinished : Maybe msg
    , breathingSpeed : Milliseconds
    }
    -> Model msg
init props =
    Model
        { currentBreath = 1
        , breathingSpeed = props.breathingSpeed
        , bubbleType = props.bubbleType
        , onFinished = props.onFinished
        }


tickSpeed : Model msg -> Float
tickSpeed (Model model) =
    model.breathingSpeed
        |> Millis.toInt
        |> (*) 2
        |> toFloat


withSpeed : Milliseconds -> Model msg -> Model msg
withSpeed millis (Model model) =
    Model { model | breathingSpeed = millis }


withBreathCount : Int -> Model msg -> Model msg
withBreathCount breaths (Model model) =
    Model { model | bubbleType = Counting breaths }



--- Update ---


type Msg
    = Tick
    | Reset


update :
    { msg : Msg
    , model : Model msg
    , toModel : Model msg -> model
    }
    -> ( model, Effect msg )
update props =
    let
        (Model model) =
            props.model

        toParentModel : ( Model msg, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            Tick ->
                let
                    ( nextBreath, breathingFinished ) =
                        case model.bubbleType of
                            Static ->
                                ( model.currentBreath, False )

                            Counting maxBreaths ->
                                if model.currentBreath == maxBreaths then
                                    ( 1, True )

                                else
                                    ( model.currentBreath + 1, False )
                in
                ( Model { model | currentBreath = nextBreath }
                , if breathingFinished then
                    case model.onFinished of
                        Nothing ->
                            Effect.none

                        Just msg ->
                            Effect.sendMsg msg

                  else
                    Effect.none
                )

            Reset ->
                ( Model { model | currentBreath = 1 }
                , Effect.none
                )



--- View ---


view : BreathingBubble msg -> Element msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        speed =
            model.breathingSpeed |> Millis.toInt
    in
    el
        [ width <| px settings.size
        , height <| px settings.size
        , centerX
        , centerY
        ]
    <|
        Utils.animatedEl
            (Animation.steps
                { startAt = [ P.scale 0.07 ]
                , options =
                    [ Animation.easeOutQuad
                    , Animation.loop
                    ]
                }
                [ Animation.step speed [ P.scale 1 ]
                , Animation.step speed [ P.scale 0.07 ]
                ]
            )
            [ Font.bold
            , width <| px settings.size
            , height <| px settings.size
            , Border.rounded <| settings.size // 2
            , Font.color settings.bgColor
            , BG.color settings.bubbleColor
            ]
        <|
            el
                [ centerX
                , centerY
                , Font.size <|
                    case settings.fontSize of
                        Nothing ->
                            settings.size // 2

                        Just size ->
                            size
                ]
            <|
                text <|
                    case settings.label of
                        Nothing ->
                            model.currentBreath |> String.fromInt

                        Just label ->
                            label
