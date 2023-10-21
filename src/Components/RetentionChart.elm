module Components.RetentionChart exposing (new, view)

import Chart as C
import Chart.Attributes as CA
import Element exposing (..)
import Lib.Utils as Utils
import Svg


type RetentionChart
    = Settings
        { width : Int
        , height : Int
        , meanRetentionColor : Color
        , maxRetentionColor : Color
        , copyColor : Color
        , meanRetentionTimes : List Int
        , maxRetention : Int
        }


new :
    { width : Int
    , height : Int
    , meanRetentionColor : Color
    , maxRetentionColor : Color
    , copyColor : Color
    , meanRetentionTimes : List Int
    , maxRetention : Int
    }
    -> RetentionChart
new props =
    Settings
        { width = props.width
        , height = props.height
        , meanRetentionColor = props.meanRetentionColor
        , maxRetentionColor = props.maxRetentionColor
        , copyColor = props.copyColor
        , meanRetentionTimes = props.meanRetentionTimes
        , maxRetention = props.maxRetention
        }


view : RetentionChart -> Element msg
view (Settings settings) =
    let
        labelWidth =
            --- So basically this value is found by trial-and-error, and
            --- it doesn't look right on eg. Chrome on desktop. No idea
            --- how to get elm-charts to work with elm-ui properly...
            37

        max =
            settings.maxRetention |> toFloat

        data =
            settings.meanRetentionTimes
                |> List.indexedMap (\i d -> { x = toFloat i, y = toFloat d })
    in
    el
        [ width <| px <| settings.width - (labelWidth * 2)
        , height <| px settings.height
        ]
        (C.chart
            [ CA.width <| toFloat <| settings.width - (labelWidth * 2)
            , CA.height 200

            -- , CA.margin { left = labelWidth, right = labelWidth, top = 0, bottom = 0 }
            -- , CA.padding { left = 0, right = 0, top = 0, bottom = 0 }
            , CA.domain
                [ CA.lowest 0 CA.orLower
                , CA.highest (max + (max / 7)) CA.orHigher
                ]
            ]
            [ C.generate (List.length settings.meanRetentionTimes)
                C.ints
                .x
                []
              <|
                \plane int ->
                    [ C.xTick
                        [ CA.x (toFloat int)
                        , CA.color <| Utils.colorToHex settings.copyColor
                        , CA.noGrid
                        ]
                    ]
            , C.series .x
                [ C.interpolated .y
                    [ CA.opacity 0.6
                    , CA.gradient []
                    , CA.monotone
                    , CA.color <| Utils.colorToHex settings.meanRetentionColor
                    ]
                    []
                ]
                data
            , C.withPlane <|
                \p ->
                    [ C.line
                        [ CA.x1 p.x.min
                        , CA.y1 max
                        , CA.x2 p.x.max
                        , CA.color <| Utils.colorToHex settings.maxRetentionColor
                        , CA.width 2
                        ]
                    ]
            , C.yLabel
                [ CA.x 0
                , CA.y max
                , CA.color <| Utils.colorToHex settings.copyColor
                , CA.ellipsis labelWidth 50
                ]
                [ Svg.text <| Utils.formatSeconds <| round max ]
            ]
            |> html
        )
