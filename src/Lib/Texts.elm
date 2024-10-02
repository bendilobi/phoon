module Lib.Texts exposing (..)


type AppLanguage
    = En
    | De


motivationHeading : AppLanguage -> String
motivationHeading lang =
    case lang of
        En ->
            "Find Motivation"

        De ->
            "Motivation finden"
