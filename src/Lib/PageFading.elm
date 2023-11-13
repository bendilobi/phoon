module Lib.PageFading exposing (..)

import Element exposing (..)
import Element.Background as BG
import Lib.ColorScheme as CS
import Simple.Transition as Transition


type FadeState
    = PreparingFadeIn Color
    | FadingIn Color
    | PreparingFadeOut
    | FadingOut Color


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


fadeOverlay : FadeState -> Element msg
fadeOverlay fadeState =
    el
        ((htmlAttribute <|
            Transition.properties
                [ Transition.opacity duration [ Transition.easeInOutCirc ] --Transition.easeInOutQuint ] -- Transition.easeInQuart ]
                ]
         )
            :: (case fadeState of
                    PreparingFadeIn color ->
                        [ BG.color color
                        , alpha 1
                        ]

                    FadingIn color ->
                        [ BG.color color
                        , alpha 0
                        ]

                    PreparingFadeOut ->
                        [ alpha 0 ]

                    FadingOut color ->
                        [ BG.color color
                        , alpha 1
                        ]
               )
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
