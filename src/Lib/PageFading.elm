module Lib.PageFading exposing
    ( FadeState
    , Trigger(..)
    , duration
    , fadeOverlay
    , init
    , initCmd
    , sessionFadingColor
    , update
    , updateCmd
    )

import Delay
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


init : Trigger -> FadeState
init trigger =
    case trigger of
        NoFade ->
            PreparingFadeOut

        FadeWith clr ->
            PreparingFadeIn clr


initCmd : Trigger -> (Trigger -> msg) -> Cmd msg
initCmd trigger msg =
    case trigger of
        FadeWith color ->
            --- For some reason, the transition animation doesn't play
            --- without the delay:
            Delay.after 50 <| msg <| FadeWith color

        NoFade ->
            Cmd.none


update : Trigger -> FadeState
update trigger =
    case trigger of
        FadeWith color ->
            FadingIn color

        NoFade ->
            PreparingFadeOut


updateCmd : Trigger -> (Trigger -> msg) -> Cmd msg
updateCmd trigger msg =
    case trigger of
        FadeWith color ->
            Delay.after duration <| msg NoFade

        NoFade ->
            Cmd.none


duration =
    700


sessionFadingColor : Color
sessionFadingColor =
    CS.primaryColors.primary


fadeOverlay : Trigger -> FadeState -> Element msg
fadeOverlay fadeOutTrigger state =
    let
        fadeState =
            case fadeOutTrigger of
                FadeWith color ->
                    FadingOut color

                NoFade ->
                    state
    in
    el
        -- ((htmlAttribute <| Transition.properties [ Transition.opacity duration [ Transition.easeInOutCirc ] ])
        --     ::
        ((case fadeState of
            PreparingFadeIn color ->
                [ BG.color color
                , alpha 1
                ]

            FadingIn color ->
                [ BG.color color
                , alpha 0
                , htmlAttribute <| Transition.properties [ Transition.opacity duration [ Transition.easeInCirc ] ]
                ]

            PreparingFadeOut ->
                [ alpha 0 ]

            FadingOut color ->
                [ BG.color color
                , alpha 1
                , htmlAttribute <| Transition.properties [ Transition.opacity duration [ Transition.easeOutCirc ] ]
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
