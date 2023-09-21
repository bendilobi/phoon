module Lib.ColorScheme exposing (primary)

import Element exposing (..)
import Element.Background as BG
import Element.Font as Font


hmg :
    { primary : Color
    , primaryDark : Color
    , primaryDarker : Color
    , primaryLight : Color
    , primaryLighter : Color
    , primaryLightest : Color
    , secondary : Color
    , secondaryDark : Color
    , secondaryDarker : Color
    , secondaryLight : Color
    , secondaryLighter : Color
    , secondaryLightest : Color
    , white : Color
    , orange : Color
    , yellow : Color
    , yellowGreen : Color
    , magenta : Color
    , green : Color
    , red : Color
    , grey : Color
    , copy : Color
    }
hmg =
    { primary = rgb255 132 184 255
    , primaryDark = rgb255 36 149 208
    , primaryDarker = rgb255 0 122 194
    , primaryLight = rgb255 170 213 241
    , primaryLighter = rgb255 220 241 252
    , primaryLightest = rgb255 243 251 254
    , secondary = rgb255 177 179 179
    , secondaryDark = rgb255 135 137 136
    , secondaryDarker = rgb255 85 87 86
    , secondaryLight = rgb255 216 218 218
    , secondaryLighter = rgb255 236 236 236
    , secondaryLightest = rgb255 245 246 246
    , white = rgb255 255 255 255
    , orange = rgb255 244 142 0
    , yellow = rgb255 255 228 0
    , yellowGreen = rgb255 166 199 0
    , magenta = rgb255 236 92 156
    , green = rgb255 101 186 128
    , red = rgb255 233 46 20
    , grey = rgb255 236 236 236
    , copy = rgb255 0 0 0
    }


primary : Color
primary =
    hmg.primary



--TODO: Wie will ich dieses Modul strukturieren? wie oben oder wie unten? Mischung?
--      Oder: "Neutral" benannte Farben als Record, App-spezifische Benennungen als
--      Union Type + Funktion, die die Zuordnung macht?
--      Also: Record enthält das Farbschema, der Union Type sagt, welche funktionalen
--      Farben es gibt (z.B. Deactivated, RetentionPhase), die Funktion sagt, wie
--      die Schema-Farben zugeordnet werden


type ColorPalette
    = Primary
    | PrimaryDark
    | PrimaryDarker
    | PrimaryLight
    | PrimaryLighter
    | PrimaryLightest
    | Secondary
    | SecondaryDark
    | SecondaryDarker
    | SecondaryLight
    | SecondaryLighter
    | SecondaryLightest
    | White
    | Grey
    | Copy
    | Warning


colorPalette : ColorPalette -> Color
colorPalette color =
    case color of
        Primary ->
            rgb255 132 184 255

        PrimaryDark ->
            rgb255 36 149 208

        PrimaryDarker ->
            rgb255 0 122 194

        PrimaryLight ->
            rgb255 170 213 241

        PrimaryLighter ->
            rgb255 220 241 252

        PrimaryLightest ->
            rgb255 243 251 254

        Secondary ->
            rgb255 177 179 179

        SecondaryDark ->
            rgb255 135 137 136

        SecondaryDarker ->
            rgb255 85 87 86

        SecondaryLight ->
            rgb255 216 218 218

        SecondaryLighter ->
            rgb255 236 236 236

        SecondaryLightest ->
            rgb255 245 246 246

        White ->
            rgb255 255 255 255

        Grey ->
            rgb255 236 236 236

        Copy ->
            rgb255 0 0 0

        Warning ->
            rgb255 233 46 20


primaryDark : List (Attribute msg)
primaryDark =
    [ BG.color <| colorPalette PrimaryDark
    , Font.color <| colorPalette White
    ]
