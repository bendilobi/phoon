module Version exposing (appVersion)

{-| The app version is the basis for our updating mechanism. There is a file /version/version.json
that MUST contain the same string as defined here. The app checks whether the strings are
different by fetching the version.json. If they are different, an update button is shown which
triggers the update process. During that, a page reload is triggered repeatedly until the version
strings match.
-}


appVersion =
    --- Version string in version.json MUST BE IDENTICAL before deploying the app!!! ---
    "0.9.42"
