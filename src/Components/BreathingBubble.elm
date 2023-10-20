module Components.BreathingBubble exposing
    ( BreathingBubble
    , BubbleType(..)
    , Model
    , Msg(..)
    , init
    , new
    , update
    , view
    , withFontSize
    , withLabel
    )

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font



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


type BreathType
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
        , bubbleType : BubbleType
        , onFinished : Maybe msg
        }


starting =
    AtBreath 1 In


init :
    { bubbleType : BubbleType
    , onFinished : Maybe msg
    }
    -> Model msg
init props =
    Model
        { breathingState = starting
        , bubbleType = props.bubbleType
        , onFinished = props.onFinished
        }



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
        ([ Font.bold
         , width <| px settings.size
         , height <| px settings.size
         , Border.rounded <| settings.size // 2
         , centerX
         , centerY
         ]
            ++ (case model.breathingState of
                    AtBreath _ In ->
                        [ Font.color settings.bgColor
                        , BG.color settings.bubbleColor
                        ]

                    _ ->
                        [ Font.color settings.bubbleColor
                        , BG.color settings.bgColor
                        ]
               )
        )
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
