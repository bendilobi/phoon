module Lib.Swipe exposing
    ( onMove, onEnd, onStart, onEndWithOptions
    , Gesture, Event, blanco, record
    , Position, locate, deltaX, deltaY, isTap, isUpSwipe, isDownSwipe, isLeftSwipe, isRightSwipe
    , isDownFlick, isLeftFlick, isRightFlick, isUpFlick, maxFingers, onCancel
    )

{-| Early stages of gesture recognition for touch-events.

This is intended to be used in qualified form.


# Hooking it up

In your model:

    { gesture = Swipe.Gesture }

In your init:

    { gesture = Swipe.blanco }

In your Msg:

    type Msg
        = Swipe Swipe.Event
        | SwipeEnd Swipe.Event

In your view:

    Html.div
        [ Swipe.onStart Swipe
        , Swipe.onMove Swipe
        , Swipe.onEnd SwipeEnd
        ]
        [ Html.text "Swipe me!" ]

In your update:

    Swipe touch ->
        { model | gesture = Swipe.record touch model.gesture }

    SwipeEnd touch ->
        let
            gesture : Swipe.Gesture
            gesture =
                Swipe.record touch model.gesture

            -- use inspection functions like `isTap` and `isLeftSwipe`
        in
        { model | gesture = Swipe.blanco }


# Events stuff

@docs onMove, onEnd, onStart, onEndWithOptions


# Keep some state around

@docs Gesture, Event, blanco, record


# Get yourself some info

@docs Position, locate, deltaX, deltaY, isTap, isUpSwipe, isDownSwipe, isLeftSwipe, isRightSwipe

-}

import Html
import Html.Events exposing (custom, on)
import Json.Decode as Json exposing (Decoder)
import List.Extra
import Time


{-| Checks if a given gesture is actually a (single) tap
-}
isTap : Gesture -> Bool
isTap gesture =
    case gesture of
        EndTap _ _ ->
            True

        _ ->
            False


{-| For a finished move, checks how much you move horizontally, from start to
finish.
-}
deltaX : Gesture -> Maybe Float
deltaX gesture =
    case gesture of
        EndGesture _ { from, to } ->
            Just (to.x - from.x)

        _ ->
            Nothing


{-| For a finished move, checks how much you move vertically, from start to
finish.
-}
deltaY : Gesture -> Maybe Float
deltaY gesture =
    case gesture of
        EndGesture _ { from, to } ->
            Just (to.y - from.y)

        _ ->
            Nothing


{-| Is this gesture finished and did we move more than `sensitivity`
(difference between `touchstart` and `touchend` in px) to the right?
-}
isRightSwipe : Float -> Gesture -> Bool
isRightSwipe sensitivity =
    isSwipeType deltaX (\dX -> dX >= sensitivity)


{-| Is this gesture finished and did we move more than `sensitivity`
(difference between `touchstart` and `touchend` in px) to the left?
-}
isLeftSwipe : Float -> Gesture -> Bool
isLeftSwipe sensitivity =
    isSwipeType deltaX (\dX -> dX <= -sensitivity)


{-| Is this gesture finished and did we move more than `sensitivity`
(difference between `touchstart` and `touchend` in px) to the bottom?
-}
isDownSwipe : Float -> Gesture -> Bool
isDownSwipe sensitivity =
    isSwipeType deltaY (\dY -> dY >= sensitivity)


{-| Is this gesture finished and did we move more than `sensitivity`
(difference between `touchstart` and `touchend` in px) to the top?
-}
isUpSwipe : Float -> Gesture -> Bool
isUpSwipe sensitivity =
    isSwipeType deltaY (\dY -> dY <= -sensitivity)


isSwipeType : (Gesture -> Maybe Float) -> (Float -> Bool) -> Gesture -> Bool
isSwipeType delta predicate =
    delta >> Maybe.map predicate >> Maybe.withDefault False


isRightFlick : Gesture -> Bool
isRightFlick =
    isFlickType isRightSwipe


isLeftFlick : Gesture -> Bool
isLeftFlick =
    isFlickType isLeftSwipe


isDownFlick : Gesture -> Bool
isDownFlick =
    isFlickType isDownSwipe


isUpFlick : Gesture -> Bool
isUpFlick =
    isFlickType isUpSwipe


isFlickType : (Float -> Gesture -> Bool) -> Gesture -> Bool
isFlickType predicate =
    flickGesture >> Maybe.map (predicate 20) >> Maybe.withDefault False



--TODO: Funktionsweise der Flick-Implementierung dokumentieren
--TODO: Flick-Implementierung vervollständigen: Andere Richtungen


flickGesture : Gesture -> Maybe Gesture
flickGesture gesture =
    case gesture of
        EndGesture fingers trail ->
            let
                newThrough =
                    trail.to.time
                        |> Maybe.map Time.posixToMillis
                        |> Maybe.map (\t -> t - 50)
                        |> Maybe.map
                            (\t ->
                                List.Extra.takeWhile
                                    (\x ->
                                        x.time
                                            |> Maybe.map Time.posixToMillis
                                            |> Maybe.map (\st -> st > t)
                                            --TODO: Passt das wenn keine Zeit vorhanden?
                                            |> Maybe.withDefault False
                                    )
                                    trail.through
                            )
            in
            newThrough
                |> Maybe.andThen List.Extra.last
                --TODO: through should not be an empty list, but it isn't used at the moment, so
                --      let's implement that later...
                |> Maybe.map (\p -> EndGesture fingers { from = p, through = [], to = trail.to })

        _ ->
            Nothing


maxFingers : Gesture -> Int
maxFingers gesture =
    case gesture of
        None ->
            0

        Started fingers _ ->
            fingers

        Moved fingers _ ->
            fingers

        EndGesture fingers _ ->
            fingers

        EndTap fingers _ ->
            fingers


{-| A position, similar to the one in the `elm-lang/mouse` package.
-}
type alias Position =
    { x : Float, y : Float, time : Maybe Time.Posix }


type alias Trail =
    { from : Position, through : List Position, to : Position }


{-| A `Gesture`! You'll want to keep one of these around in your model and
update it whenever applicable.
-}
type Gesture
    = None
    | Started Int Position
    | Moved Int Trail
    | EndGesture Int Trail
    | EndTap Int Position


{-| A single `Swipe.Event`. Gestures are made up of these, internally.
-}
type Event
    = Touch EventType Int Position


{-| Useful if you want to know the current position during a stream of events.
-}
locate : Event -> Position
locate (Touch _ _ position) =
    position


type EventType
    = Start
    | Move
    | End


{-| Get yourself a blanco gesture, as if no touches have happened at all.

After a touchend event, you'll probably want to reset to this, too.

-}
blanco : Gesture
blanco =
    None


addToTrail : Position -> Trail -> Trail
addToTrail coordinate { from, to, through } =
    { from = from, through = to :: through, to = coordinate }


{-| Our cute little `update`-like function!
-}
record : Event -> Maybe Time.Posix -> Gesture -> Gesture
record (Touch eventType fingers position) time gesture =
    let
        coordinate =
            { position | time = time }
    in
    case ( eventType, gesture ) of
        ( Start, _ ) ->
            Started fingers coordinate

        ( Move, Started fingrs prev ) ->
            Moved (max fingers fingrs) { from = prev, through = [], to = coordinate }

        ( Move, Moved fingrs trail ) ->
            addToTrail coordinate trail |> Moved (max fingers fingrs)

        ( Move, _ ) ->
            Started fingers coordinate

        ( End, Moved fingrs trail ) ->
            addToTrail coordinate trail |> EndGesture (max fingers fingrs)

        ( End, Started fingrs _ ) ->
            EndTap (max fingers fingrs) coordinate

        ( End, _ ) ->
            EndTap fingers coordinate


decodeTouch : String -> (Position -> msg) -> Decoder msg
decodeTouch fieldName tagger =
    Json.map3 Position
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
        (Json.succeed Nothing)
        |> Json.at [ fieldName, "0" ]
        |> Json.map tagger


decodeTouchWithOptions : String -> { stopPropagation : Bool, preventDefault : Bool } -> (Position -> msg) -> Decoder { message : msg, preventDefault : Bool, stopPropagation : Bool }
decodeTouchWithOptions fieldName options tagger =
    Json.map3 (\x y -> Position x y)
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
        (Json.succeed Nothing)
        |> Json.at [ fieldName, "0" ]
        |> Json.map (\p -> { message = tagger p, preventDefault = options.preventDefault, stopPropagation = options.stopPropagation })


decodeTouchListLength : String -> Decoder Int
decodeTouchListLength fieldName =
    Json.field "length" Json.int
        |> Json.at [ fieldName ]


{-| Record the start of a touch gesture.
-}
onStart : (Event -> msg) -> Html.Attribute msg
onStart tagger =
    decodeTouchListLength "touches"
        |> Json.andThen (\length -> decodeTouch "touches" (Touch Start length >> tagger))
        |> on "touchstart"


{-| Record an ongoing touch gesture.
-}
onMove : (Event -> msg) -> Html.Attribute msg
onMove tagger =
    decodeTouchListLength "changedTouches"
        |> Json.andThen (\length -> decodeTouch "changedTouches" (Touch Move length >> tagger))
        |> on "touchmove"



--TODO: do we need an onCancelWithOptions?


onCancel : (Event -> msg) -> Html.Attribute msg
onCancel tagger =
    decodeTouchListLength "changedTouches"
        |> Json.andThen (\length -> decodeTouch "changedTouches" (Touch Move length >> tagger))
        |> on "touchcancel"


{-| Record the end of a touch gesture.

**Note**: This sets `preventDefault = True` to avoid double events from occuring
when the same DOM node also has an `onClick` attribute. Using `preventDefault`
means that if a `touchend` event happens, the `onClick` handler won't fire.

If you have a case where you need to support a regular `onClick` event nested in
a node that has `onEnd` on it (for example; a container with swipe support,
which contains a button from an external package), please see `onEndWithOptions`.

-}
onEnd : (Event -> msg) -> Html.Attribute msg
onEnd =
    onEndWithOptions { stopPropagation = False, preventDefault = True }


{-| Record the end of a touch gesture with options.
-}
onEndWithOptions :
    { stopPropagation : Bool
    , preventDefault : Bool
    }
    -> (Event -> msg)
    -> Html.Attribute msg
onEndWithOptions options tagger =
    decodeTouchListLength "changedTouches"
        |> Json.andThen (\length -> decodeTouchWithOptions "changedTouches" options (Touch End length >> tagger))
        |> custom "touchend"
