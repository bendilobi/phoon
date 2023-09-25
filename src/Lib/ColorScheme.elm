module Lib.ColorScheme exposing
    ( ColorScheme
    , breathingInverted
    , guide
    , interactActive
    , interactInactive
    , navbar
    , newDaylight
    , newSunrise
    , phaseBreathing
    , phaseRelaxRetention
    , phaseRetention
    , phaseSessionEnd
    , phaseSessionStart
    , primary
    , primaryInformation
    , primaryMotivation
    , primaryPrepareSession
    , seriesBad
    , seriesGood
    , sessionStartInverted
    )

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font


type alias Colors =
    { primary : Color
    , primaryCopy : Color
    , primaryMotivation : Color
    , primaryPrepareSession : Color
    , primaryInformation : Color
    , primaryMotivationCopy : Color
    , interactActive : Color
    , interactActiveDarker : Color
    , interactActiveCopy : Color
    , interactInactive : Color
    , interactInactiveDarker : Color
    , interactInactiveCopy : Color
    , guide : Color
    , actionNeeded : Color
    , actionNeededCopy : Color
    , success : Color
    , seriesGood : Color
    , seriesBad : Color
    , phaseSessionStart : Color
    , phaseSessionStartCopy : Color
    , phaseBreathing : Color
    , phaseBreathingCopy : Color
    , phaseRetention : Color
    , phaseRetentionCopy : Color
    , phaseRelaxRetention : Color
    , phaseRelaxRetentionCopy : Color
    , phaseSessionEnd : Color
    , phaseSessionEndCopy : Color
    , navbar : Color
    , navbarBorder : Color
    , navbarCopy : Color
    }


type ColorScheme
    = ColorScheme Colors


newSunrise : ColorScheme
newSunrise =
    ColorScheme
        { primary = rgb255 4 14 30
        , primaryCopy = rgb255 241 241 230
        , primaryMotivation = rgb255 4 14 30
        , primaryPrepareSession = rgb255 21 35 65 --57 68 101
        , primaryInformation = rgb255 246 249 255 --229 238 255 --242 249 255 --241 241 230
        , primaryMotivationCopy = rgb255 241 241 230
        , interactActive = rgb255 132 110 141 --147 110 158 --255 180 93
        , interactActiveDarker = rgb255 85 77 104 --98 80 124 --237 120 105
        , interactActiveCopy = rgb255 241 241 230
        , interactInactive = rgb255 167 170 189
        , interactInactiveDarker = rgb255 115 118 135
        , interactInactiveCopy = rgb255 245 248 255
        , guide = rgb255 187 136 0
        , actionNeeded = rgb255 184 37 17
        , actionNeededCopy = rgb255 0 0 0
        , success = rgb255 0 125 84
        , seriesGood = rgb255 218 162 27
        , seriesBad = rgb255 184 37 17
        , phaseSessionStart = rgb255 105 56 112
        , phaseSessionStartCopy = rgb255 200 196 183
        , phaseBreathing = rgb255 181 78 117 --50 49 46
        , phaseBreathingCopy = rgb255 200 196 183
        , phaseRetention = rgb255 255 180 93 --38 86 86
        , phaseRetentionCopy = rgb255 12 38 84 --255 255 255
        , phaseRelaxRetention = rgb255 249 248 113 --46 69 131
        , phaseRelaxRetentionCopy = rgb255 0 148 255 --255 255 255
        , phaseSessionEnd = rgb255 0 148 255 --50 49 46
        , phaseSessionEndCopy = rgb255 255 247 214 --255 255 255
        , navbar = rgb255 0 15 8
        , navbarBorder = rgb255 24 37 68 --52 63 97
        , navbarCopy = rgb255 91 101 96
        }


newDaylight : ColorScheme
newDaylight =
    ColorScheme
        { primary = rgb255 4 14 30
        , primaryCopy = rgb255 241 241 230
        , primaryMotivation = rgb255 0 148 255
        , primaryPrepareSession = rgb255 100 129 232 --123 145 188
        , primaryInformation = rgb255 243 249 255 --229 238 255 --242 249 255 --241 241 230
        , primaryMotivationCopy = rgb255 245 249 255
        , interactActive = rgb255 155 74 146 --243 91 136
        , interactActiveDarker = rgb255 154 59 116 --182 26 87
        , interactActiveCopy = rgb255 245 249 255
        , interactInactive = rgb255 167 170 189
        , interactInactiveDarker = rgb255 115 118 135
        , interactInactiveCopy = rgb255 245 248 255
        , guide = rgb255 197 167 117 --238 232 169
        , actionNeeded = rgb255 184 37 17
        , actionNeededCopy = rgb255 0 0 0
        , success = rgb255 0 125 84
        , seriesGood = rgb255 238 232 169
        , seriesBad = rgb255 184 37 17
        , phaseSessionStart = rgb255 105 56 112
        , phaseSessionStartCopy = rgb255 200 196 183
        , phaseBreathing = rgb255 181 78 117 --50 49 46
        , phaseBreathingCopy = rgb255 200 196 183
        , phaseRetention = rgb255 255 180 93 --38 86 86
        , phaseRetentionCopy = rgb255 12 38 84 --255 255 255
        , phaseRelaxRetention = rgb255 249 248 113 --46 69 131
        , phaseRelaxRetentionCopy = rgb255 0 148 255 --255 255 255
        , phaseSessionEnd = rgb255 0 148 255 --50 49 46
        , phaseSessionEndCopy = rgb255 255 247 214 --255 255 255
        , navbar = rgb255 0 83 78
        , navbarBorder = rgb255 8 32 30
        , navbarCopy = rgb255 123 145 188 --141 149 168 --164 171 189
        }


primary : ColorScheme -> List (Attribute msg)
primary (ColorScheme colors) =
    [ BG.color colors.primary
    , Font.color colors.primaryCopy
    ]


primaryMotivation : ColorScheme -> List (Attribute msg)
primaryMotivation (ColorScheme colors) =
    [ BG.color colors.primaryMotivation
    , Font.color colors.primaryMotivationCopy
    ]


primaryPrepareSession : ColorScheme -> List (Attribute msg)
primaryPrepareSession (ColorScheme colors) =
    [ BG.color colors.primaryPrepareSession
    , Font.color colors.primaryMotivationCopy
    ]


primaryInformation : ColorScheme -> List (Attribute msg)
primaryInformation (ColorScheme colors) =
    [ BG.color colors.primaryInformation
    , Font.color colors.primary
    ]


navbar : ColorScheme -> List (Attribute msg)
navbar (ColorScheme colors) =
    [ BG.color colors.navbar
    , Font.color colors.navbarCopy
    , Border.color colors.navbarBorder
    ]


guide : ColorScheme -> Color
guide (ColorScheme colors) =
    colors.guide


seriesGood : ColorScheme -> Color
seriesGood (ColorScheme colors) =
    colors.seriesGood


seriesBad : ColorScheme -> Color
seriesBad (ColorScheme colors) =
    colors.seriesBad


interactActive : ColorScheme -> List (Attribute msg)
interactActive (ColorScheme colors) =
    [ BG.color colors.interactActive
    , Font.color colors.interactActiveCopy
    , Border.color colors.interactActiveDarker
    ]


interactInactive : ColorScheme -> List (Attribute msg)
interactInactive (ColorScheme colors) =
    [ BG.color colors.interactInactive
    , Font.color colors.interactInactiveCopy
    , Border.color colors.interactInactiveDarker
    ]


phaseSessionStart : ColorScheme -> List (Attribute msg)
phaseSessionStart (ColorScheme colors) =
    [ BG.color colors.phaseSessionStart
    , Font.color colors.phaseSessionStartCopy
    ]


sessionStartInverted : ColorScheme -> List (Attribute msg)
sessionStartInverted (ColorScheme colors) =
    [ Font.color colors.phaseSessionStart
    , BG.color colors.phaseSessionStartCopy
    ]


phaseBreathing : ColorScheme -> List (Attribute msg)
phaseBreathing (ColorScheme colors) =
    [ BG.color colors.phaseBreathing

    -- , Font.color colors.phaseBreathingCopy
    , Font.color colors.phaseSessionStart
    ]


breathingInverted : ColorScheme -> List (Attribute msg)
breathingInverted (ColorScheme colors) =
    [ Font.color colors.phaseBreathing

    -- , BG.color colors.phaseBreathingCopy
    , BG.color colors.phaseSessionStart
    ]


phaseRetention : ColorScheme -> List (Attribute msg)
phaseRetention (ColorScheme colors) =
    [ BG.color colors.phaseRetention

    -- , Font.color colors.phaseRetentionCopy
    , Font.color colors.phaseBreathing
    ]


phaseRelaxRetention : ColorScheme -> List (Attribute msg)
phaseRelaxRetention (ColorScheme colors) =
    [ BG.color colors.phaseRelaxRetention

    -- , Font.color colors.phaseRelaxRetentionCopy
    , Font.color colors.phaseRetention
    ]


phaseSessionEnd : ColorScheme -> List (Attribute msg)
phaseSessionEnd (ColorScheme colors) =
    [ BG.color colors.phaseSessionEnd

    -- , Font.color colors.phaseSessionEndCopy
    , Font.color colors.phaseRelaxRetention
    ]


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
