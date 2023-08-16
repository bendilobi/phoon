module Pages.Home_ exposing (page)

import Element exposing (..)
import Element.Background as Background
import View exposing (View)


page : View msg
page =
    { title = "Zoff - WHM the Zen Way"
    , attributes = []
    , element =
        column
            [ width fill
            , height fill
            , Background.color <| rgb255 200 196 183
            ]
            [ link
                [ centerX
                , centerY
                ]
                { url = "/breathsession"
                , label = text "Hallöchen bei Zoff!"
                }
            ]
    }
