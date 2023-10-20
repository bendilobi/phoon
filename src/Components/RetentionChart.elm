module Components.RetentionChart exposing (new, view)

import Chart
import Chart.Attributes as ChartA
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Lib.ColorScheme as CS exposing (ColorScheme)
import Lib.Utils as Utils
import Svg


type RetentionChart
    = Settings
        { width : Int
        , height : Int

        --TODO: Farben außerhalb festlegen
        -- meanRetentionColor : Color
        -- , maxRetentionColor : Color
        , meanRetentionTimes : List Int
        , maxRetention : Int
        }


new :
    { width : Int
    , height : Int

    --     meanRetentionColor : Color
    -- , maxRetentionColor : Color
    , meanRetentionTimes : List Int
    , maxRetention : Int
    }
    -> RetentionChart
new props =
    Settings
        { width = props.width
        , height = props.height

        --     meanRetentionColor = props.meanRetentionColor
        -- , maxRetentionColor = props.maxRetentionColor
        , meanRetentionTimes = props.meanRetentionTimes
        , maxRetention = props.maxRetention
        }


view : ColorScheme -> RetentionChart -> Element msg
view colorScheme (Settings settings) =
    let
        paddingX =
            --TODO: Kann man nicht doch irgendwie dafür sorgen, dass das Label
            --      innerhalb der width bleibt...?
            75

        max =
            settings.maxRetention |> toFloat

        data =
            settings.meanRetentionTimes
                |> List.indexedMap (\i d -> { x = toFloat i, y = toFloat d })
    in
    el
        [ centerX
        , width <| px <| settings.width - paddingX
        , height <| px settings.height
        ]
        (Chart.chart
            [ ChartA.width <| toFloat <| settings.width - paddingX
            , ChartA.height 200
            , ChartA.domain
                [ ChartA.lowest 0 ChartA.orLower
                , ChartA.highest (max + (max / 7)) ChartA.orHigher
                ]
            ]
            [ Chart.generate (List.length settings.meanRetentionTimes)
                Chart.ints
                .x
                []
              <|
                \plane int ->
                    [ Chart.xTick
                        [ ChartA.x (toFloat int)
                        , ChartA.color <| CS.interactInactiveDarkerColorHex colorScheme
                        , ChartA.noGrid
                        ]
                    ]
            , Chart.series .x
                [ Chart.interpolated .y
                    [ ChartA.opacity 0.6
                    , ChartA.gradient []
                    , ChartA.monotone
                    , ChartA.color <| CS.guideColorHex colorScheme
                    ]
                    []
                ]
                data
            , Chart.withPlane <|
                \p ->
                    [ Chart.line
                        [ ChartA.x1 p.x.min
                        , ChartA.y1 max
                        , ChartA.x2 p.x.max
                        , ChartA.color <| CS.seriesGoodColorHex colorScheme
                        , ChartA.width 2
                        ]
                    ]
            , Chart.yLabel
                [ ChartA.x 0
                , ChartA.y max
                , ChartA.color <| CS.interactInactiveDarkerColorHex colorScheme
                ]
                [ Svg.text <| Utils.formatSeconds <| round max ]
            ]
            |> html
        )
