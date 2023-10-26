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
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
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


type
    BreathType
    --TODO: brauche ich das ganze In-Out? Vielleicht einfach Animation Steps verwenden...
    = In
    | Out


type BreathingState
    = AtBreath Int BreathType


type BubbleType
    = Static
    | Counting Int


type Model msg
    = Model
        { breathingState : BreathingState
        , breathingSpeed : Milliseconds
        , bubbleType : BubbleType
        , onFinished : Maybe msg
        }


starting =
    AtBreath 1 In


init :
    { bubbleType : BubbleType
    , onFinished : Maybe msg
    , breathingSpeed : Milliseconds
    }
    -> Model msg
init props =
    Model
        { breathingState = starting
        , breathingSpeed = props.breathingSpeed
        , bubbleType = props.bubbleType
        , onFinished = props.onFinished
        }


tickSpeed : Model msg -> Float
tickSpeed (Model model) =
    model.breathingSpeed
        |> Millis.toInt
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
                case model.breathingState of
                    AtBreath n In ->
                        case model.bubbleType of
                            Static ->
                                ( Model { model | breathingState = AtBreath n Out }
                                , Effect.none
                                )

                            Counting maxBreaths ->
                                if n == maxBreaths then
                                    ( Model { model | breathingState = starting }
                                    , case model.onFinished of
                                        Nothing ->
                                            Effect.none

                                        Just msg ->
                                            Effect.sendMsg msg
                                    )

                                else
                                    ( Model { model | breathingState = AtBreath n Out }
                                    , Effect.none
                                    )

                    AtBreath n Out ->
                        case model.bubbleType of
                            Static ->
                                ( Model { model | breathingState = AtBreath n In }
                                , Effect.none
                                )

                            Counting maxBreaths ->
                                if n < maxBreaths then
                                    ( Model { model | breathingState = AtBreath (n + 1) In }
                                    , Effect.none
                                    )

                                else
                                    ( Model { model | breathingState = starting }
                                    , case model.onFinished of
                                        Nothing ->
                                            Effect.none

                                        Just msg ->
                                            Effect.sendMsg msg
                                    )

            Reset ->
                ( Model { model | breathingState = starting }
                , Effect.none
                )



--- View ---


view : BreathingBubble msg -> Element msg
view (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    el
        [ width <| px settings.size
        , height <| px settings.size
        , centerX
        , centerY
        ]
    <|
        animatedEl
            (case model.breathingState of
                AtBreath _ In ->
                    --TODO: Dafür sorgen, dass beim ersten view noch nicht animiert wird
                    --      Starting State wieder einführen?
                    Animation.fromTo
                        { duration = model.breathingSpeed |> Millis.toInt
                        , options = [ Animation.easeOutQuad ]
                        }
                        [ P.scale 0.1 ]
                        [ P.scale 1 ]

                AtBreath _ Out ->
                    Animation.fromTo
                        { duration = model.breathingSpeed |> Millis.toInt
                        , options = [ Animation.easeOutQuad ]
                        }
                        [ P.scale 1 ]
                        [ P.scale 0.1 ]
            )
            [ Font.bold
            , width <| px settings.size
            , height <| px settings.size
            , Border.rounded <| settings.size // 2
            , Font.color settings.bgColor
            , BG.color settings.bubbleColor
            ]
        <|
            case model.breathingState of
                AtBreath n _ ->
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
                                    String.fromInt n

                                Just label ->
                                    label



--TODO: Das ist aus der Doku von Simple Animation kopiert... in Utils verschieben?


animatedUi =
    Animated.ui
        { behindContent = behindContent
        , htmlAttribute = htmlAttribute
        , html = html
        }


animatedEl : Animation -> List (Attribute msg) -> Element msg -> Element msg
animatedEl =
    animatedUi el


animatedColumn : Animation -> List (Attribute msg) -> List (Element msg) -> Element msg
animatedColumn =
    animatedUi column
