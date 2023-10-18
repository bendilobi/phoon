module Components.BreathingBubble exposing
    ( BreathingBubble
    , BubbleType(..)
    , Model
    , Msg(..)
    , init
    , new
    , update
    , view
    , withLabel
    )

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Lib.ColorScheme as CS exposing (ColorScheme)



--- Settings ---


type BreathingBubble msg
    = Settings
        { model : Model msg
        , size : Int
        , label : Maybe String
        }


new :
    { model : Model msg
    , size : Int
    }
    -> BreathingBubble msg
new props =
    Settings
        { model = props.model
        , size = props.size
        , label = Nothing
        }



--- Modifiers ---


withLabel : String -> BreathingBubble msg -> BreathingBubble msg
withLabel label (Settings settings) =
    Settings { settings | label = Just label }



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


view : ColorScheme -> BreathingBubble msg -> Element msg
view colorScheme (Settings settings) =
    let
        (Model model) =
            settings.model
    in
    el
        ([ Font.bold
         , width <| px settings.size
         , height <| px settings.size
         , Border.rounded <| settings.size // 2
         , Border.width 1
         , centerX
         , centerY
         ]
            ++ (case model.breathingState of
                    AtBreath _ In ->
                        CS.breathingInverted colorScheme

                    _ ->
                        []
               )
        )
    <|
        case model.breathingState of
            AtBreath n _ ->
                el
                    [ centerX
                    , centerY
                    , Font.size <| settings.size // 3
                    ]
                <|
                    text <|
                        case settings.label of
                            Nothing ->
                                String.fromInt n

                            Just label ->
                                label
