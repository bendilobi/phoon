module Lib.ColorScheme exposing
    ( ColorScheme
    , actionNeededColor
    , greyOverTransparencyColor
    , guideColor
    , interactActive
    , interactActiveColor
    , interactActiveLighter
    , interactActiveLighterColor
    , interactInactive
    , interactInactiveDarkerColor
    , navbar
    , newDaylight
    , newSunrise
    , phaseBreathing
    , phaseBreathingColor
    , phaseRelaxRetention
    , phaseRelaxRetentionColor
    , phaseRetention
    , phaseSessionEnd
    , phaseSessionStart
    , phaseSessionStartColor
    , phaseSessionStartCopyColor
    , primary
    , primaryColors
    , primaryInformation
    , primaryMotivation
    , primaryMotivationCopyColor
    , primaryPrepareSession
    , seriesBadColor
    , seriesGoodColor
    , settingsColor
    , settingsDarkerColor
    , successColor
    )

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font


type ColorScheme
    = ColorScheme
        { primaryMotivation : Color
        , primaryPrepareSession : Color
        , primaryInformation : Color
        , primaryMotivationCopy : Color
        , interactActive : Color
        , interactActiveLighter : Color
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
        , settings : Color
        , settingsDarker : Color
        , greyOverTransparency : Color
        }


newSunrise : ColorScheme
newSunrise =
    ColorScheme
        { primaryMotivation = rgb255 4 14 30
        , primaryPrepareSession = rgb255 21 35 65 --57 68 101
        , primaryInformation = rgb255 246 249 255 --229 238 255 --242 249 255 --241 241 230
        , primaryMotivationCopy = rgb255 241 241 230
        , interactActive = rgb255 132 110 141 --147 110 158 --255 180 93
        , interactActiveLighter = rgb255 243 91 136
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
        , settings = rgb 1 1 1 --rgb 0.9 0.9 0.9
        , settingsDarker = rgb 0.7 0.7 0.7
        , greyOverTransparency = rgb 0.35 0.35 0.35
        }


newDaylight : ColorScheme
newDaylight =
    ColorScheme
        { primaryMotivation = rgb255 0 148 255
        , primaryPrepareSession = rgb255 100 129 232 --123 145 188
        , primaryInformation = rgb255 243 249 255 --229 238 255 --242 249 255 --241 241 230
        , primaryMotivationCopy = rgb255 245 249 255
        , interactActive = rgb255 155 74 146 --243 91 136
        , interactActiveLighter = rgb255 243 91 136
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
        , navbarBorder = rgb255 0 121 113 --8 32 30
        , navbarCopy = rgb255 120 139 138 --120 126 139 --101 106 117 --123 145 188 --141 149 168 --164 171 189
        , settings = rgb 1 1 1 --rgb 0.9 0.9 0.9
        , settingsDarker = rgb 0.7 0.7 0.7
        , greyOverTransparency = rgb 0.55 0.55 0.55
        }


primaryColors : { primary : Color, font : Color, border : Color }
primaryColors =
    { primary = rgb255 4 14 30
    , font = rgb255 241 241 230
    , border = rgb 0 0 0
    }


primary : List (Attribute msg)
primary =
    [ BG.color primaryColors.primary
    , Font.color primaryColors.font
    , Border.color primaryColors.border
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
    , Font.color primaryColors.primary
    ]


navbar : ColorScheme -> List (Attribute msg)
navbar (ColorScheme colors) =
    [ BG.color colors.navbar
    , Font.color colors.navbarCopy
    , Border.color colors.navbarBorder
    ]


guideColor : ColorScheme -> Color
guideColor (ColorScheme colors) =
    colors.guide


seriesGoodColor : ColorScheme -> Color
seriesGoodColor (ColorScheme colors) =
    colors.seriesGood


seriesBadColor : ColorScheme -> Color
seriesBadColor (ColorScheme colors) =
    colors.seriesBad


interactActiveColor : ColorScheme -> Color
interactActiveColor (ColorScheme colors) =
    colors.interactActive


interactActiveLighterColor : ColorScheme -> Color
interactActiveLighterColor (ColorScheme colors) =
    colors.interactActiveLighter


interactInactiveDarkerColor : ColorScheme -> Color
interactInactiveDarkerColor (ColorScheme colors) =
    colors.interactInactiveDarker


interactActive : ColorScheme -> List (Attribute msg)
interactActive (ColorScheme colors) =
    [ BG.color colors.interactActive
    , Font.color colors.interactActiveCopy
    , Border.color colors.interactActiveDarker
    ]


interactActiveLighter : ColorScheme -> List (Attribute msg)
interactActiveLighter (ColorScheme colors) =
    [ BG.color colors.interactActiveLighter
    , Font.color colors.interactActiveCopy
    , Border.color colors.interactActive
    ]


interactInactive : ColorScheme -> List (Attribute msg)
interactInactive (ColorScheme colors) =
    [ BG.color colors.interactInactive
    , Font.color colors.interactInactiveCopy
    , Border.color colors.interactInactiveDarker
    ]


primaryMotivationCopyColor : ColorScheme -> Color
primaryMotivationCopyColor (ColorScheme colors) =
    colors.primaryMotivationCopy


phaseSessionStart : ColorScheme -> List (Attribute msg)
phaseSessionStart (ColorScheme colors) =
    [ BG.color colors.phaseSessionStart
    , Font.color colors.phaseSessionStartCopy
    ]


phaseSessionStartCopyColor : ColorScheme -> Color
phaseSessionStartCopyColor (ColorScheme colors) =
    colors.phaseSessionStartCopy


phaseSessionStartColor : ColorScheme -> Color
phaseSessionStartColor (ColorScheme colors) =
    colors.phaseSessionStart


phaseBreathing : ColorScheme -> List (Attribute msg)
phaseBreathing (ColorScheme colors) =
    [ BG.color colors.phaseBreathing

    -- , Font.color colors.phaseBreathingCopy
    , Font.color colors.phaseSessionStart
    ]


phaseBreathingColor : ColorScheme -> Color
phaseBreathingColor (ColorScheme colors) =
    colors.phaseBreathing


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


phaseRelaxRetentionColor : ColorScheme -> Color
phaseRelaxRetentionColor (ColorScheme colors) =
    colors.phaseRelaxRetention


settingsColor : ColorScheme -> Color
settingsColor (ColorScheme colors) =
    colors.settings


settingsDarkerColor : ColorScheme -> Color
settingsDarkerColor (ColorScheme colors) =
    colors.settingsDarker


greyOverTransparencyColor : ColorScheme -> Color
greyOverTransparencyColor (ColorScheme colors) =
    colors.greyOverTransparency


successColor : ColorScheme -> Color
successColor (ColorScheme colors) =
    colors.success


actionNeededColor : ColorScheme -> Color
actionNeededColor (ColorScheme colors) =
    colors.actionNeeded
