module Lib.PageFading exposing (..)

import Element exposing (..)
import Element.Background as BG
import Lib.ColorScheme as CS
import Simple.Transition as Transition


type FadeState
    = PreparingFadeIn
    | FadingIn
    | PreparingFadeOut
    | FadingOut


type Trigger
    = NoFade
    | FadeWith Color


duration =
    500


sessionFadingColor : Color
sessionFadingColor =
    CS.primaryColors.primary



--TODO: Unterschiedliche Geschwindigkeiten -> Müsste als Parameter ans Layout gegeben werden
-- slow =
--     1000


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
                [ Transition.opacity duration [ Transition.easeInOutCirc ] --Transition.easeInOutQuint ] -- Transition.easeInQuart ]
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
