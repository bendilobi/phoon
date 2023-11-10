module Lib.PageFading exposing (..)

import Element exposing (..)
import Element.Background as BG
import Simple.Transition as Transition


type FadeState
    = PreparingFadeIn
    | FadingIn
    | PreparingFadeOut
    | FadingOut


fadeDuration =
    500


fadeOverlay : Color -> FadeState -> Element msg
fadeOverlay color fadeState =
    el
        ([ BG.color color
         , case fadeState of
            PreparingFadeIn ->
                alpha 1

            FadingIn ->
                alpha 0

            PreparingFadeOut ->
                alpha 0

            FadingOut ->
                alpha 1
         , htmlAttribute <|
            Transition.properties
                [ Transition.opacity fadeDuration [ Transition.easeInOutCirc ] --Transition.easeInOutQuint ] -- Transition.easeInQuart ]
                ]
         ]
            ++ (case fadeState of
                    PreparingFadeOut ->
                        [ width <| px 0
                        , height <| px 0
                        ]

                    _ ->
                        [ width fill
                        , height fill
                        ]
               )
        )
        none
