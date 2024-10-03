module Lib.Texts exposing (..)

import Element exposing (..)
import Element.Font as Font
import String.Format
import Time exposing (Weekday(..))


type AppLanguage
    = En
    | De


appName : String
appName =
    "Zoff"


motivationHeading : AppLanguage -> String
motivationHeading lang =
    case lang of
        De ->
            "Motivation finden"

        _ ->
            "Find Motivation"


daysSinceLastPractice : AppLanguage -> Int -> String
daysSinceLastPractice lang n =
    case lang of
        De ->
            if n == 1 then
                "Tag seit der letzten Übung"

            else
                "Tage seit der letzten Übung"

        _ ->
            if n == 1 then
                "Day since last practice"

            else
                "Days since last practice"


letsGo : AppLanguage -> String
letsGo lang =
    case lang of
        De ->
            "Auf geht's!"

        _ ->
            "Let's go!"


practiceToday : AppLanguage -> String
practiceToday lang =
    case lang of
        De ->
            "Praktiziere noch heute, um Deine Serie fortzusetzen!"

        _ ->
            "Practice today to continue your streak!"


welcome : AppLanguage -> String
welcome lang =
    case lang of
        De ->
            "Willkommen bei {{ }}!!"
                |> String.Format.value appName

        _ ->
            "Welcome to {{ }}!!"
                |> String.Format.value appName


welcome2 : AppLanguage -> String
welcome2 lang =
    case lang of
        De ->
            "Herzlich willkommen bei {{ }}!!"
                |> String.Format.value appName

        _ ->
            "Welcome to {{ }}!!"
                |> String.Format.value appName


introduction : AppLanguage -> String
introduction lang =
    case lang of
        De ->
            """
            Mit {{ }} machst Du Deine Atemübung ganz entspannt, vielleicht sogar im Liegen und mit geschlossenen
            Augen - Klänge leiten Dich jeweils zum nächsten Schritt. Und wenn Du selbst entscheiden möchtest, wann es 
            weitergeht (z.B. Beginn und Ende der Retention), tippst Du einfach mit drei Fingern irgendwo auf den Bildschirm.
            """
                |> String.Format.value appName

        _ ->
            """With {{ }}, you do your breathing exercise in a completely relaxed manner, perhaps even lying down and with your eyes closed.
            eyes closed - sounds guide you to the next step. And if you want to decide for yourself when to continue 
            (e.g. start and end of retention), simply tap anywhere on the screen with three fingers."""
                |> String.Format.value appName


installInstruction : AppLanguage -> Element msg -> List (Element msg)
installInstruction lang icon =
    case lang of
        De ->
            [ text """Diese App ist darauf optimiert, als Standalone-Link auf dem Smartphone-Homebildschirm 
                            installiert zu werden. Auf dem iPhone musst Du dafür Safari nutzen und im "Teilen"-Menü ("""
            , icon
            , text """) "Zum Home-Bildschirm" wählen."""
            ]

        _ ->
            [ text """Diese App ist darauf optimiert, als Standalone-Link auf dem Smartphone-Homebildschirm 
                            installiert zu werden. Auf dem iPhone musst Du dafür Safari nutzen und im "Teilen"-Menü ("""
            , icon
            , text """) "Zum Home-Bildschirm" wählen."""
            ]


introduction2 : AppLanguage -> String
introduction2 lang =
    case lang of
        De ->
            """
            {{ }} hilft Dir auch dabei, eine regelmäßige Übungspraxis aufrechtzuerhalten: Hier wird erfasst, wie oft Du in Serie
            Die Atemübungen gemacht hast. Und unter "Optimieren" kannst Du festlegen, wie oft pro Woche Du üben willst - so 
            kannst Du auch hier und dort mal einen Tag auslassen, ohne Deine Serie zu verlieren!
            """
                |> String.Format.value appName

        _ ->
            """
            {{ }} also helps you to maintain a regular exercise practice: This records how many times in a row
            you have done the breathing exercises. And under "Optimise" you can specify how often you want to practise each week - so you can 
            This allows you to skip a day here and there without losing your series!
            """
                |> String.Format.value appName


streakInfoHeader : AppLanguage -> String
streakInfoHeader lang =
    case lang of
        De ->
            "Deine Serie"

        _ ->
            "Your Streak"


lastStreak : AppLanguage -> String
lastStreak lang =
    case lang of
        De ->
            "Letzte Serie"

        _ ->
            "Last Streak"


longestStreak : AppLanguage -> String
longestStreak lang =
    case lang of
        De ->
            "Längste Serie"

        _ ->
            "Longest Streak"


currentStreak : AppLanguage -> String
currentStreak lang =
    case lang of
        De ->
            "Aktuelle Serie"

        _ ->
            "Current Streak"


practiceUntilLongest : AppLanguage -> String
practiceUntilLongest lang =
    case lang of
        De ->
            "Übe noch {{ }} mal und Du hast Deine längste Serie eingeholt!"

        _ ->
            "Practice {{ }} more times and you will have caught up with your longest streak!"


caughtUpLongest : AppLanguage -> String
caughtUpLongest lang =
    case lang of
        De ->
            "Du hast gerade Deine längste Serie bisher! Super!!"

        _ ->
            "You are having your longest streak so far! Great!!"


practiceUntilLast : AppLanguage -> String
practiceUntilLast lang =
    case lang of
        De ->
            "Übe noch {{ }} mal und Du hast Deine letzte Serie eingeholt!"

        _ ->
            "Practice {{ }} more times and you will have caught up with your most recent streak!"


caughtUpLast : AppLanguage -> String
caughtUpLast lang =
    case lang of
        De ->
            "Du hast gerade Deine letzte Serie eingeholt! Super!!"

        _ ->
            "You caught up with your most recent streak! Great!!"


practiceTomorrow : AppLanguage -> List (Element msg)
practiceTomorrow lang =
    case lang of
        De ->
            [ text "Um die Serie fortzusetzen, übe "
            , el [ Font.bold ] <| text "spätestens morgen "
            , text "wieder!"
            ]

        _ ->
            [ text "To continue your streak, practice again "
            , el [ Font.bold ] <| text "tomorrow at the latest."
            ]


weekday : AppLanguage -> Time.Weekday -> String
weekday lang day =
    case lang of
        De ->
            case day of
                Mon ->
                    "Montag"

                Tue ->
                    "Dienstag"

                Wed ->
                    "Mittwoch"

                Thu ->
                    "Donnerstag"

                Fri ->
                    "Freitag"

                Sat ->
                    "Samstag"

                Sun ->
                    "Sonntag"

        _ ->
            case day of
                Mon ->
                    "Monday"

                Tue ->
                    "Tuesday"

                Wed ->
                    "Wednesday"

                Thu ->
                    "Thursday"

                Fri ->
                    "Friday"

                Sat ->
                    "Saturday"

                Sun ->
                    "Sunday"


practiceUntilWeekday : AppLanguage -> Time.Weekday -> Bool -> List (Element msg)
practiceUntilWeekday lang wday nextWeek =
    case lang of
        De ->
            [ text "Um die Serie fortzusetzen, übe spätestens am "
            , el [ Font.bold ] <| text <| weekday lang wday
            , if nextWeek then
                text " nächste Woche"

              else
                none
            ]

        _ ->
            [ text "To continue your streak, practice by "
            , el [ Font.bold ] <| text <| weekday lang wday
            , if nextWeek then
                text " next week"

              else
                none
            , text " at the latest."
            ]


nextRingAfter : AppLanguage -> Int -> String
nextRingAfter lang sessions =
    case lang of
        De ->
            "Der nächste Ring kommt nach "
                ++ (if sessions > 1 then
                        String.fromInt sessions ++ " Übungen."

                    else
                        "der nächsten Übung."
                   )

        _ ->
            "You will get your next ring after "
                ++ (if sessions > 1 then
                        String.fromInt sessions ++ " practice sessions."

                    else
                        "your next practice session."
                   )


lastPracticeWas : AppLanguage -> String
lastPracticeWas lang =
    case lang of
        De ->
            "Deine letzte Übung war vor {{ }} Tagen."

        _ ->
            "Your last practice session was {{ }} days ago."


updating : AppLanguage -> String
updating lang =
    case lang of
        De ->
            "Aktualisiere..."

        _ ->
            "Updating..."


updateSuccessfull : AppLanguage -> String
updateSuccessfull lang =
    case lang of
        De ->
            "Update auf Version {{ }} erfolgreich!"

        _ ->
            "Update to version {{ }} successfull!"


done : AppLanguage -> String
done lang =
    case lang of
        De ->
            "Fertig"

        _ ->
            "Done"


tryLater : AppLanguage -> String
tryLater lang =
    case lang of
        De ->
            "Später versuchen"

        _ ->
            "Try again later"


motivate : AppLanguage -> String
motivate lang =
    case lang of
        De ->
            "Motivieren"

        _ ->
            "Motivate"


practice : AppLanguage -> String
practice lang =
    case lang of
        De ->
            "Praktizieren"

        _ ->
            "Practice"


optimize : AppLanguage -> String
optimize lang =
    case lang of
        De ->
            "Optimieren"

        _ ->
            "Optimize"


back : AppLanguage -> String
back lang =
    case lang of
        De ->
            "Zurück"

        _ ->
            "Back"


prepareSession : AppLanguage -> String
prepareSession lang =
    case lang of
        De ->
            "Sitzung vorbereiten"

        _ ->
            "Prepare session"


cycles : AppLanguage -> Int -> List (Element msg)
cycles lang n =
    case lang of
        De ->
            [ el [ Font.bold ] <| text <| String.fromInt n
            , text " Runde"
            , el [ transparent <| n == 1 ] <| text "n"
            ]

        _ ->
            [ el [ Font.bold ] <| text <| String.fromInt n
            , text " cycle"
            , el [ transparent <| n == 1 ] <| text "s"
            ]


estimatedEnd : AppLanguage -> Element msg -> List (Element msg)
estimatedEnd lang time =
    case lang of
        De ->
            [ text "Geschätztes Ende: "
            , el [ Font.bold, Font.size 30 ] time
            , text " Uhr"
            ]

        _ ->
            [ text "Estimated end: "
            , el [ Font.bold, Font.size 30 ] time
            , text " o'clock"
            ]


warnings : AppLanguage -> String
warnings lang =
    case lang of
        De ->
            "Warnhinweise"

        _ ->
            "Warnings"


practiceWarnings : AppLanguage -> String
practiceWarnings lang =
    case lang of
        De ->
            "Auf KEINEN Fall im Wasser üben!!!"

        _ ->
            "Don't ever practice in water!!!"


startSession : AppLanguage -> String
startSession lang =
    case lang of
        De ->
            "Los geht's!"

        _ ->
            "Let's go!"
