module Lib.Texts exposing (..)

import Element exposing (..)
import Element.Font as Font
import Html exposing (th)
import Lib.Millis as Millis exposing (Milliseconds)
import Lib.Session as Session exposing (BreathingSpeed(..))
import String exposing (fromInt)
import String.Format
import Time exposing (Weekday(..))



--- Technical stuff


type AppLanguage
    = En
    | De


boldify : List (Attribute msg) -> String -> List (Element msg)
boldify attrs txt =
    let
        startsWithBold =
            String.left 1 txt == "*"

        makeEl : Bool -> String -> Element msg
        makeEl bold strg =
            if bold then
                el (Font.bold :: attrs) <| text strg

            else
                text strg

        applyBold : String -> ( Bool, List (Element msg) ) -> ( Bool, List (Element msg) )
        applyBold strg ( isBold, result ) =
            if strg == "" then
                {- If the txt begins with an "*", String.split has the first list entry
                   be a "" which we ignore:
                -}
                ( isBold, result )

            else
                ( not isBold, result ++ [ makeEl isBold strg ] )
    in
    txt
        |> String.split "*"
        |> List.foldl applyBold ( startsWithBold, [] )
        |> Tuple.second


bulletParagraph : List (Element msg) -> Element msg
bulletParagraph content =
    row [ spacing 8, paddingXY 20 0 ]
        [ el [ alignTop, Font.bold ] <| text "•"
        , paragraph
            [ Font.alignLeft

            --- This is a bugfix for (it seems) a bug in elm-ui...
            --- See https://github.com/mdgriffith/elm-ui/issues/124
            --- Without this, the button that is overlayed on swipe in the
            --- SessionControls is not clickable at first, only on the second
            --- tap...
            -- , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
            ]
            content
        ]


bullet : String -> Element msg
bullet content =
    content
        |> boldify []
        |> bulletParagraph


para : String -> Element msg
para content =
    paragraph [] (content |> boldify [])


viewTime : AppLanguage -> List (Attribute msg) -> Time.Zone -> Time.Posix -> Element msg
viewTime lang attrs zone time =
    let
        hour =
            Time.toHour zone time

        minute =
            Time.toMinute zone time
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    case lang of
        De ->
            el attrs <| text <| (hour |> String.fromInt) ++ ":" ++ minute

        _ ->
            if hour > 12 then
                paragraph []
                    [ el attrs <| text <| (hour - 12 |> String.fromInt) ++ ":" ++ minute
                    , text " pm"
                    ]

            else
                paragraph []
                    [ el attrs <| text <| (hour |> String.fromInt) ++ ":" ++ minute
                    , text " am"
                    ]



--- App Name


appName : String
appName =
    "Phoon"



--- TEXTS AND TRANSLATIONS ---


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
                "day since last practice"

            else
                "days since last practice"


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
    (case lang of
        De ->
            "Herzlich willkommen bei {{ }}!"

        _ ->
            "Welcome to {{ }}!"
    )
        |> String.Format.value appName


introduction : AppLanguage -> List (Element msg)
introduction lang =
    case lang of
        De ->
            [ para
                ("""
                *{{}}* ist eine App für Smartphones (oder andere Touch Devices), die Dich die positiven Effekte der *Wim Hof Atemübung* 
                so bequem wie möglich genießen lässt.                
                """
                    |> String.Format.value appName
                )
            , bullet
                --TODO: Doch irgendwie das mit den Ringen unterbringen?
                ("""
                *Finde Motivation* für eine regelmäßige Übungspraxis: Bestimme, wie häufig Du üben willst - {{ }} erfasst, wie lange Du Deine
                Serie durchhältst.
                """
                    |> String.Format.value appName
                )
            , bullet
                --TODO: Zeitprognose und ad hoc Anpassung erwähnen?
                ("""
                *Praktiziere, wie es für Dich am bequemsten ist* - im Sitzen oder im Liegen: {{ }} führt Dich mit Klängen und Animationen und lässt sich 
                während der Übung komplett mit Berührungsgesten steuern.
                """
                    |> String.Format.value appName
                )
            , bullet """
                *Optimiere Deinen Übungserfolg*: Verfolge die Entwicklung Deiner Retentionszeiten und passe die Übungsparameter entsprechend an.
                """
            ]

        _ ->
            [ para
                ("""*{{ }}* is an app for smartphones (or other touch devices) that lets you reap the benefits of *Wim Hof style breathwork*
                as conveniently as possible.
                """
                    |> String.Format.value appName
                )
            , bullet
                ("""
            *Find and maintain motivation* for regular practice: Set a goal of how often you intend to practice - {{ }} keeps track of your streak
            and makes sure you don't miss a beat
                """
                    |> String.Format.value appName
                )
            , bullet
                ("""
            *Conveniently practice however you like* - even lying down with closed eyes: {{ }} guides you with sounds and animations and can be  
            operated during the practice session entirely by touch gestures.
                """
                    |> String.Format.value appName
                )
            , bullet """
            *Optimize your breathwork success*: fine-tune your exercise parameters and keep track of your retention time progress.
            """
            ]



--TODO: Kurzer allgemeiner Installationshinweis


installInstruction : AppLanguage -> Element msg -> List (Element msg)
installInstruction lang icon =
    case lang of
        De ->
            [ para ("""
            {{ }} ist eine "Progressive Web Application (PWA)" - eine Webseite, die darauf optimiert ist, wie eine App aus dem 
            App Store auszusehen und benutzt zu werden. Um das bestmöglich erleben zu können, installiere sie bitte auf dem 
            Home-Screen: Öffne das Teilen-Menü (
            """ |> String.Format.value appName)
            , icon
            , para """ Icon) und wähle dort "Zum Home-Bildschirm"."""
            ]

        _ ->
            [ para ("""{{ }} is a "Progressive Web Application (PWA)" - a website that's made to look and feel like a native app from the 
            App Store. For this experience, please install it as a "link" to your phone's Home screen: Go to the share menu (
            """ |> String.Format.value appName)
            , icon
            , para """ icon) and choose "Add to Home Screen" in the list that appears."""
            ]


introduction2 : AppLanguage -> List (Element msg)
introduction2 lang =
    case lang of
        De ->
            [ para
                ("""
                {{ }} bietet nur grundlegende Hinweise zur Durchführung der Wim-Hof-Atemtechnik. Wenn Du Dich damit noch nicht
                auskennst, schaue Dir am Besten eines der unzähligen Erklärvideos auf YouTube an. Eine Erklärung vom Meister selbst
                ist besser als tausend Worte geschriebene Anleitung!
            """
                    |> String.Format.value appName
                )
            ]

        _ ->
            [ para
                ("""
            {{ }} provides only basic instructions on how to do Wim Hof style breathwork. If you're new to his breathwork, we recommend you
            head over to YouTube and find one of the numerous videos on the topic. Being shown by the man himself is surely better than
            a thousand words of explanation!
            """ |> String.Format.value appName)
            ]


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


practiceUntilLongest : AppLanguage -> Int -> String
practiceUntilLongest lang n =
    String.Format.value (String.fromInt n) <|
        case lang of
            De ->
                "Übe noch {{ }} mal und Du hast Deine längste Serie eingeholt!"

            _ ->
                if n == 1 then
                    "Practice one more time and you will have caught up with your longest streak!"

                else
                    "Practice {{ }} more times and you will have caught up with your longest streak!"


caughtUpLongest : AppLanguage -> String
caughtUpLongest lang =
    case lang of
        De ->
            "*Du hast gerade Deine längste Serie bisher! Fantastisch!!*"

        _ ->
            "*You are having your longest streak so far! Phantastic!!*"


practiceUntilLast : AppLanguage -> Int -> String
practiceUntilLast lang n =
    String.Format.value (String.fromInt n) <|
        case lang of
            De ->
                "Übe noch {{ }} mal und Du hast Deine letzte Serie eingeholt!"

            _ ->
                if n == 1 then
                    "Practice one more time and you will have caught up with your last streak!"

                else
                    "Practice {{ }} more times and you will have caught up with your last streak!"


caughtUpLast : AppLanguage -> String
caughtUpLast lang =
    case lang of
        De ->
            "Du hast gerade Deine letzte Serie eingeholt! Super!!"

        _ ->
            "You caught up with your last streak! Great!!"


practiceTomorrow : AppLanguage -> List (Element msg)
practiceTomorrow lang =
    (case lang of
        De ->
            "Um die Serie fortzusetzen übe *spätestens morgen* wieder!"

        _ ->
            "To continue your streak, practice again *tomorrow at the latest*"
    )
        |> boldify []


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
            "Um die Serie fortzusetzen, übe spätestens am *{{ }}{{ }}*!"
                |> String.Format.value (weekday lang wday)
                |> String.Format.value
                    (if nextWeek then
                        " nächste Woche"

                     else
                        ""
                    )
                |> boldify []

        _ ->
            "To continue your streak, practice by *{{ }} {{ }}* at the latest!"
                |> String.Format.value (weekday lang wday)
                |> String.Format.value
                    (if nextWeek then
                        "next week"

                     else
                        ""
                    )
                |> boldify []


maxRingsReached : AppLanguage -> Int -> String
maxRingsReached lang n =
    String.Format.value (String.fromInt n) <|
        case lang of
            De ->
                "Du hast gerade die maximal mögliche Anzahl von Ringen ({{ }})."

            _ ->
                "You have reached the maximum possible number of rings ({{ }})."


nextRingAfter : AppLanguage -> Int -> String
nextRingAfter lang sessions =
    String.Format.value (String.fromInt sessions) <|
        case lang of
            De ->
                if sessions == 1 then
                    "Der nächste Ring kommt nach der nächsten Übung"

                else
                    "Der nächste Ring kommt nach {{ }} Übungen"

            _ ->
                if sessions == 1 then
                    "You will geht the next ring after your next practice session."

                else
                    "You will geht your next ring after {{ }} practice sessions."


lastPracticeWas : AppLanguage -> Int -> String
lastPracticeWas lang n =
    String.Format.value (String.fromInt n) <|
        case lang of
            De ->
                if n == 1 then
                    "Deine letzte Übung war gestern."

                else
                    "Deine letzte Übung war vor {{ }} Tagen."

            _ ->
                if n == 1 then
                    "Your last practice session was yesterday."

                else
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


cycles : AppLanguage -> Int -> List (Attribute msg) -> List (Element msg)
cycles lang n nAttributes =
    case lang of
        De ->
            [ el nAttributes <| text <| String.fromInt n
            , text " Runde"
            , el [ transparent <| n == 1 ] <| text "n"
            ]

        _ ->
            [ el nAttributes <| text <| String.fromInt n
            , text " cycle"
            , el [ transparent <| n == 1 ] <| text "s"
            ]


cycles2 : AppLanguage -> Int -> String
cycles2 lang n =
    String.Format.value (String.fromInt n) <|
        case lang of
            De ->
                if n == 1 then
                    "{{ }} Runde"

                else
                    "{{ }} Runden"

            _ ->
                if n == 1 then
                    "{{ }} Cycle"

                else
                    "{{ }} Cycles"


estimatedEnd : AppLanguage -> Element msg -> List (Element msg)
estimatedEnd lang time =
    case lang of
        De ->
            [ text "Geschätztes Ende: "
            , time
            , text " Uhr"
            ]

        _ ->
            [ text "Estimated end: "
            , time
            ]


warnings : AppLanguage -> String
warnings lang =
    case lang of
        De ->
            "Sicherheitshinweise"

        _ ->
            "Safety instructions"


practiceWarnings : AppLanguage -> List (Element msg)
practiceWarnings lang =
    case lang of
        De ->
            [ para """
            Die Atemübung, wie sie von Wim Hof weltweit bekannt gemacht wurde, ist eine intensive, körperliche Übung, die den Organismus
            gezielt einer Belastung aussetzt (Sauerstoff-Entzug), um einen Anpassungseffekt zu bewirken. Es kann dabei in seltenen Fällen
            während der Übung zu einer Bewusstseinseintrübung bis hin zu einer kurzen Bewusstlosigkeit kommen.
            """
            , para """
            Es ist daher *sehr wichtig*, dass Du die Übung immer in einer Umgebung ausführst, in der das kein Problem ist, am Besten *im Sitzen 
            oder im Liegen auf einer weichen Unterlage*.
            """
            , bullet "Übe *niemals im oder am Wasser* (auch nicht im flachen Wasser)!"
            , bullet "Übe *nie, während Du Auto fährst*"
            , bullet "Übe nicht, wenn Du nicht körperlich fit bist."
            , bullet """Im Falle von Herzproblemen, Bluthochdruck oder anderen Herz-Kreislauf-Problemen, Schwangerschaft, Epilepsie,
            oder ähnlichen Gesundheitseinschränkungen, *konsultiere auf jeden Fall Deinen Arzt*, bevor Du die Atemübung ausprobierst!
            """
            , para """
            Die Autoren dieser App übernehmen keine Verantwortung für Deinen Umgang mit der App, d.h. Du übst auf eigene Verantwortung! 
            """
            , para """
            Informiere Dich bitte selbst über die korrekte Ausführung der Wim Hof Atmung (z.B. im Internet, auf Youtube, ...).
            """
            ]

        _ ->
            [ para """
            The breathing exercise, as made famous worldwide by Wim Hof, is an intense, physical exercise that exposes the organism
            to stress (oxygen deprivation) in order to bring about an adaptation effect. In rare cases, this can
            lead to a clouding or even brief loss of consciousness during the exercise.
            """
            , para """
            It is therefore *very important* that you always do the exercise in an environment where this is not a problem, preferably *sitting 
            or lying down on a soft surface*.
            """
            , bullet """*Never* practice *in or near water* (not even in shallow water)!"""
            , bullet """*Never* practice *while driving*"""
            , bullet """Do not practice if you are not physically fit."""
            , bullet """In case of heart conditions, high blood pressure or other cardiovascular issues, pregnancy, 
            epilepsy, or similar conditions, always *consult your healthcare professional* before trying the breathing exercise!
            """
            , para """
            The authors of this app accept no responsibility for your use of the app, i.e. you practise at your own risk! 
            """
            , para """
            Please inform yourself about the correct execution of Wim Hof breathing (e.g. on the Internet, on YouTube, ...).
            """
            ]


startSession : AppLanguage -> String
startSession lang =
    case lang of
        De ->
            "Los geht's!"

        _ ->
            "Let's go!"


optimizePractice : AppLanguage -> String
optimizePractice lang =
    case lang of
        De ->
            "Übung optimieren"

        _ ->
            "Optimize practice"


retentionTrend : AppLanguage -> String
retentionTrend lang =
    case lang of
        De ->
            "Retentionstrend"

        _ ->
            "Retention trend"


retTrentCaption : AppLanguage -> String
retTrentCaption lang =
    case lang of
        De ->
            """Verlauf der gemittelten Retentionsdauern pro Sitzung (letzte 30 Sitzungen). 
        Die Linie oben zeigt die bisher längste Retention."""

        _ ->
            """Plot of the average retention times per session (last 30 sessions). 
        The line at the top shows the longest retention to date"""


updateAvailable : AppLanguage -> String
updateAvailable lang =
    case lang of
        De ->
            "Ein Update ist verfügbar von Version {{ currentVersion }} auf {{ newestVersion }}"

        _ ->
            "An update is available from version {{ currentVersion }} to {{ newestVersion }}"


updateNow : AppLanguage -> String
updateNow lang =
    case lang of
        De ->
            "Update jetzt laden"

        _ ->
            "Update now"


customizeExercise : AppLanguage -> String
customizeExercise lang =
    case lang of
        De ->
            "Übung anpassen"

        _ ->
            "Customize exercise"


reset : AppLanguage -> String
reset lang =
    case lang of
        De ->
            "Zurücksetzen"

        _ ->
            "Reset"


overallSequence : AppLanguage -> String
overallSequence lang =
    case lang of
        De ->
            "Gesamtablauf"

        _ ->
            "Overall sequence"


breathingSpeed : AppLanguage -> String
breathingSpeed lang =
    case lang of
        De ->
            "Atemgeschwindigkeit"

        _ ->
            "Breathing speed"


breathingSpeeds : AppLanguage -> Session.BreathingSpeed -> String
breathingSpeeds lang speed =
    case speed of
        Slow ->
            case lang of
                De ->
                    "Langsam"

                _ ->
                    "Slow"

        Medium ->
            case lang of
                De ->
                    "Mittel"

                _ ->
                    "Medium"

        Fast ->
            case lang of
                De ->
                    "Schnell"

                _ ->
                    "Fast"


breaths : AppLanguage -> String
breaths lang =
    case lang of
        De ->
            "Atemzüge"

        _ ->
            "Breaths"


breathingDuration : AppLanguage -> Milliseconds -> List (Attribute msg) -> List (Element msg)
breathingDuration lang millis attrs =
    case lang of
        De ->
            "Dauer der Atemphase: *{{ }}* {{ }}"
                |> String.Format.value (Millis.toString False millis)
                |> String.Format.value
                    (if Millis.toSeconds millis < 60 then
                        "Sekunden"

                     else
                        "Minuten"
                    )
                |> boldify attrs

        _ ->
            "Duration of the breathing phase: *{{ }}* {{ }}"
                |> String.Format.value (Millis.toString False millis)
                |> String.Format.value
                    (if Millis.toSeconds millis < 60 then
                        "Seconds"

                     else
                        "Minutes"
                    )
                |> boldify attrs


relaxRetention : AppLanguage -> String
relaxRetention lang =
    case lang of
        De ->
            "Erholungsretention"

        _ ->
            "Relax retention"


seconds : AppLanguage -> String
seconds lang =
    case lang of
        De ->
            "Sekunden"

        _ ->
            "Seconds"


estimatedDuration : AppLanguage -> Milliseconds -> List (Attribute msg) -> List (Element msg)
estimatedDuration lang millis attrs =
    case lang of
        De ->
            "Geschätzte Gesamtdauer der Übung: *{{ }}* {{ }}"
                |> String.Format.value (Millis.toString False millis)
                |> String.Format.value
                    (if Millis.toMinutes millis < 60 then
                        "Minuten"

                     else
                        "Stunden"
                    )
                |> boldify attrs

        _ ->
            "Estimated overall duration: *{{ }}* {{ }}"
                |> String.Format.value (Millis.toString False millis)
                |> String.Format.value
                    (if Millis.toMinutes millis < 60 then
                        "Minutes"

                     else
                        "Hours"
                    )
                |> boldify attrs


practiceGoal : AppLanguage -> String
practiceGoal lang =
    case lang of
        De ->
            "Übungsziel"

        _ ->
            "Practice goal"


timesPerWeek : AppLanguage -> Int -> List (Attribute msg) -> List (Element msg)
timesPerWeek lang n attrs =
    boldify attrs <|
        String.Format.value (String.fromInt n) <|
            case lang of
                De ->
                    "*{{ }}* mal pro Woche"

                _ ->
                    if n == 1 then
                        "*{{ }}* time per week"

                    else
                        "*{{ }}* times per week"


timesPerWeek2 : AppLanguage -> Int -> String
timesPerWeek2 lang n =
    String.Format.value (String.fromInt n) <|
        case lang of
            De ->
                if n == 1 then
                    "Einmal pro Woche"

                else
                    "{{ }} mal pro Woche"

            _ ->
                if n == 1 then
                    "Once per week"

                else
                    "{{ }} times per week"


practiceGoalCaption : AppLanguage -> String
practiceGoalCaption lang =
    case lang of
        De ->
            """Das Übungsziel bestimmt, wie häufig "Schutzringe" für die Fortsetzung der Serie hinzukommen. "4 mal pro Woche" 
            bedeutet beispielsweise, dass für vier Übungen drei Ringe hinzukommen. Es können also drei von sieben Tagen freigenommen werden."""

        _ ->
            """The practice goal determines how often “protective rings” for the continuation of the series are added. “4 times per week” 
            means, for example, that three rings are added for four practice sessions. This way, three out of seven days can be taken off."""


endStreak : AppLanguage -> String
endStreak lang =
    case lang of
        De ->
            "Serie beenden?"

        _ ->
            "End streak?"


endStreak2 : AppLanguage -> String
endStreak2 lang =
    case lang of
        De ->
            "Serie beenden"

        _ ->
            "End streak"


endStreakMessage : AppLanguage -> String
endStreakMessage lang =
    case lang of
        De ->
            """Wenn Du das Übungsziel niedriger ansetzt als zu Beginn Deiner aktuellen Serie ({{ }} Tage pro Woche), 
            wird sie beendet und mit der nächsten Übung beginnt eine neue Serie!
        """

        _ ->
            """
            If you set the practice goal lower than at the beginning of your current streak ({{ }} days per week), 
        the streak will be cancelled and a new one will start with your next practice session!
        """


leaveGoalAt : AppLanguage -> String
leaveGoalAt lang =
    case lang of
        De ->
            "Ziel auf {{ }} lassen"

        _ ->
            "Leave goal at {{ }}"


importData : AppLanguage -> String
importData lang =
    case lang of
        De ->
            "Daten importieren?"

        _ ->
            "Import data?"


imp : AppLanguage -> String
imp lang =
    case lang of
        De ->
            "Importieren"

        _ ->
            "Import"


discard : AppLanguage -> String
discard lang =
    case lang of
        De ->
            "Verwerfen"

        _ ->
            "Discard"


pasteImpossible : AppLanguage -> String
pasteImpossible lang =
    case lang of
        De ->
            "Einfügen nicht möglich"

        _ ->
            "Pasting impossible"


pasteImpossibleMessage : AppLanguage -> String
pasteImpossibleMessage lang =
    case lang of
        De ->
            "Das scheinen leider keine validen Ergebnisdaten zu sein..."

        _ ->
            "This does not seems to be valid data..."


close : AppLanguage -> String
close lang =
    case lang of
        De ->
            "Schließen"

        _ ->
            "Close"


copySuccessful : AppLanguage -> String
copySuccessful lang =
    case lang of
        De ->
            "Kopieren erfolgreich"

        _ ->
            "Copying successful"


copySuccessfulMessage : AppLanguage -> String
copySuccessfulMessage lang =
    case lang of
        De ->
            "Deine Übungsergebnisse wurden in die Zwischenablage kopiert und stehen zum Einfügen in anderen Apps zur Verfügung!"

        _ ->
            "Your practice results have been copied to the clipboard and are available for pasting into other apps!"


appInfo : AppLanguage -> String
appInfo lang =
    case lang of
        De ->
            "App Info"

        _ ->
            "App Info"


appSlogan : AppLanguage -> String
appSlogan lang =
    --TODO: Besseren Slogan ohne Zen?
    case lang of
        De ->
            "Wim Hof Atmung mit dem Hauch von Zen"

        _ ->
            "Wim Hof style breathwork with a flavor of Zen"


version : AppLanguage -> String
version lang =
    case lang of
        De ->
            "Version {{ }}"

        _ ->
            "Version {{ }}"


authorAndContact : AppLanguage -> List (Element msg)
authorAndContact lang =
    case lang of
        De ->
            [ para "Diese App wurde mit Hingebung für Dich programmiert von Benno Dielmann."
            , para "Hast Du Fragen, Verbesserungsvorschläge, Ideen, Kritik? Schreibe mir eine E-Mail:"
            ]

        _ ->
            [ para "This app was programmed with dedication for you by Benno Dielmann."
            , para "Do you have any questions, suggestions for improvement, ideas or criticism? Write me an e-mail:"
            ]


dataManagement : AppLanguage -> String
dataManagement lang =
    case lang of
        De ->
            "Datenmanagement"

        _ ->
            "Data management"


copyResults : AppLanguage -> String
copyResults lang =
    case lang of
        De ->
            "Übungsergebnisse kopieren"

        _ ->
            "Copy practice results"


importResults : AppLanguage -> String
importResults lang =
    case lang of
        De ->
            "Übungsergebnisse importieren"

        _ ->
            "Import practice results"


reloadApp : AppLanguage -> String
reloadApp lang =
    case lang of
        De ->
            "App neu laden"

        _ ->
            "Reload app"


practiceSetup : AppLanguage -> String
practiceSetup lang =
    case lang of
        De ->
            "Übungssituation einrichten"

        _ ->
            "Set up practice situation"


sessionStartHints : AppLanguage -> List (Element msg)
sessionStartHints lang =
    case lang of
        De ->
            [ para """
        Nimm' eine entspannte Position ein, entweder im Sitzen oder im Liegen. Sorge dafür, dass Du 
        für die Dauer der Übung ungestört bist und entferne ggf. die Stummschaltung Deines Geräts, um 
        die Klänge hören zu können."""
            , bullet "Wische mit einem Finger nach rechts, um Optionen anzuzeigen."
            , bullet "Tippe mit einem Finger, um ein Glöckchen zu hören (Soundtest)."
            , bullet "Tippe mit drei Fingern, um mit der Atemübung zu beginnen."
            ]

        _ ->
            [ para """
        Take a relaxed position, either sitting or lying down. Make sure that you are  
        undisturbed for the duration of the exercise and, if necessary, unmute your device to be able to hear the sounds. 
        """
            , bullet "Swipe right with one finger to access the options."
            , bullet "Tap with one finger to hear a bell (for sound check)."
            , bullet "Tap or swipe with three fingers to start the breathwork session."
            ]


breathingHints : AppLanguage -> List (Element msg)
breathingHints lang =
    case lang of
        De ->
            -- [ paragraph [ paddingXY 30 0 ] [ text "Atme tief ein und aus im Rhythmus der Animation, bis die Glocke erklingt." ]
            [ bullet "Atme tief ein und aus im Rhythmus der Animation, bis die Glocke erklingt."
            , bullet """
            Beim Einatmen drücke das Zwerchfell nach unten und fülle Deine Lunge von unten beginnend maximal mit Luft.
            """
            , bullet """
            Beim Ausatmen entspanne einfach und lass' los, sodass die Luft von selbst entweicht.
            """
            ]

        _ ->
            [ bullet "Breathe in and out very deeply following the rhythm of the animation, until the bell sounds."
            , bullet """
            When inhaling, push down your diaphragm and fill your lung completely with air, beginning from the bottom.
            """
            , bullet """
            When exhaling, just relax and let go. Let the air get out all by itself.
            """
            ]


breathingEndHints : AppLanguage -> List (Element msg)
breathingEndHints lang =
    case lang of
        De ->
            [ bullet "Atme noch einmal tief ein und lass' den Atem los,"
            , bullet "dann halte die Luft an,"
            , bullet "und tippe mit drei Fingern um den Timer für die Retention zu starten."
            ]

        _ ->
            [ bullet "Breathe in very deeply one last time, then let your breath go,"
            , bullet "hold your breath,"
            , bullet "then tap with three fingers to start the retention counter."
            ]


retentionHints : AppLanguage -> List (Element msg)
retentionHints lang =
    case lang of
        De ->
            [ bullet "Halte die Luft an so lange es geht."
            , bullet "Entspanne alle Muskeln, fühle und genieße!"
            , bullet "Wenn Du nicht mehr kannst, atme einmal tief ein, halte die Luft an und tippe mit drei Fingern."
            ]

        _ ->
            [ bullet "Hold your breath as long as you comfortably are able to."
            , bullet "Relax all muscles, feel and enjoy!"
            , bullet "When you can't hold any longer, inhale very deeply, hold your breath and tap with three fingers."
            ]


relaxretHints : AppLanguage -> List (Element msg)
relaxretHints lang =
    case lang of
        De ->
            [ paragraph [ paddingXY 30 0 ] [ text "Halte die Luft an, bis der Timer abgelaufen ist und die Glocke erklingt." ]
            ]

        _ ->
            [ paragraph [ paddingXY 30 0 ] [ text "Hold your breath until the timer stops and the bell sounds." ]
            ]


sessionEndHints : AppLanguage -> List (Element msg)
sessionEndHints lang =
    case lang of
        De ->
            [ bullet "Tippe mit drei Fingern, um die Sitzungsergebnisse zu speichern und die Sitzung zu beenden."
            , bullet "Wenn Du nicht speichern möchtest, wische nach rechts, um die Optionen anzuzeigen."
            ]

        _ ->
            [ bullet "Tap with three fingers to save your retention data and end the session."
            , bullet "If you don't want to save the data, swipe right to show the options."
            ]


discardRetentionData : AppLanguage -> String
discardRetentionData lang =
    case lang of
        De ->
            "Retentionsdaten verwerfen?"

        _ ->
            "Discard retention data?"


discardRetMessage : AppLanguage -> String
discardRetMessage lang =
    case lang of
        De ->
            "Retentionsdaten aus dieser Sitzung wirklich verwerfen?"

        _ ->
            "Do you really want to discard the data from this session?"


keep : AppLanguage -> String
keep lang =
    case lang of
        De ->
            "Behalten"

        _ ->
            "Keep it"


endSession : AppLanguage -> String
endSession lang =
    case lang of
        De ->
            "Übung beenden"

        _ ->
            "End practice session"


sessionEnded : AppLanguage -> String
sessionEnded lang =
    case lang of
        De ->
            "Sitzung beendet!"

        _ ->
            "Session finished!"


cycles3 : AppLanguage -> Int -> String
cycles3 lang n =
    case lang of
        De ->
            "Runde {{ }}:"
                |> String.Format.value (String.fromInt n)

        _ ->
            "Cycle {{ }}:"
                |> String.Format.value (String.fromInt n)


mean : AppLanguage -> String
mean lang =
    case lang of
        De ->
            "Durchschnitt:"

        _ ->
            "Mean:"


oneMoreCycle : AppLanguage -> String
oneMoreCycle lang =
    case lang of
        De ->
            "Noch 'ne Runde"

        _ ->
            "Do one more cycle"


discardSession : AppLanguage -> String
discardSession lang =
    case lang of
        De ->
            "Sitzung verwerfen"

        _ ->
            "Discard session"


finish : AppLanguage -> String
finish lang =
    case lang of
        De ->
            "Beenden"

        _ ->
            "Finish"


cancelSession : AppLanguage -> String
cancelSession lang =
    case lang of
        De ->
            "Sitzung abbrechen"

        _ ->
            "Cancel session"


retention : AppLanguage -> String
retention lang =
    case lang of
        De ->
            "Retention"

        _ ->
            "Retention"


prepareRetention : AppLanguage -> String
prepareRetention lang =
    case lang of
        De ->
            "Retention vorbereiten"

        _ ->
            "Prepare retention"


breathing : AppLanguage -> String
breathing lang =
    case lang of
        De ->
            "Atmen"

        _ ->
            "Breathing"


wakeLockNote : AppLanguage -> List (Element msg)
wakeLockNote lang =
    case lang of
        De ->
            [ para """*Bitte beachten:*"""
            , para """
            Bitte stelle sicher, dass sich der Bildschirm Deines Geräts während der Atemübung nicht automatisch sperrt:
            """
            , para """
            Öffne die Einstellungen-App, gehe zu "Anzeige & Helligkeit" und wähle unter "Automatische Sperre" 4 Minuten oder mehr.
            """
            , para """
            Dieser Schritt ist leider nötig, weil die App das wegen eines Fehlers in iOS noch nicht für Dich machen kann..."
            """
            ]

        _ ->
            [ para """*Please note:*"""
            , para """
            Please make sure your screen doesn't automatically lock during your exercise:
            """
            , para """
            Open the Settings app, choose "Display & Brightness" and set "Auto-Lock" to 4 minutes or more.
            """
            , para """
            This is unfortunately necessary due to a bug in iOS which prevents apps like this one from doing it for you...
            """
            ]
