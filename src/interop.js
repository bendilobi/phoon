
const MOTIVATION_DATA_KEY = 'motivationData'
const SESSION_SETTINGS_KEY = 'sessionSettings'
const UPDATING_KEY = 'updating'
const SHOWWAKELOCKNOTE_KEY = 'showWakelockNote'
let checkVersion = () => /(iPhone|iPad) OS ([1-9]*)/g.exec(window.navigator.userAgent)?.[2] || 0
let wakeLock = null;

export const flags = ({ env }) => {
    const motivationStored = localStorage.getItem(MOTIVATION_DATA_KEY)
    const motivationJson = motivationStored ? JSON.parse(motivationStored) : null

    const sessionSettingsStored = localStorage.getItem(SESSION_SETTINGS_KEY)
    const sessionSettingsJson = sessionSettingsStored ? JSON.parse(sessionSettingsStored) : null

    const updatingStateStored = localStorage.getItem(UPDATING_KEY)
    const updatingStateJson = updatingStateStored ? JSON.parse(updatingStateStored) : null 

    const showWakelockHintStored = localStorage.getItem(SHOWWAKELOCKNOTE_KEY)
    const showWakelockHintJson = showWakelockHintStored ? JSON.parse(showWakelockHintStored) : null

    const sa = {
        sat: getComputedStyle(document.documentElement).getPropertyValue("--sat")
        , sab: getComputedStyle(document.documentElement).getPropertyValue("--sab")
        , sal: getComputedStyle(document.documentElement).getPropertyValue("--sal")
        , sar: getComputedStyle(document.documentElement).getPropertyValue("--sar")
      }

    const width = window.innerWidth
    const height = window.innerHeight

    const lang = navigator.language
    console.log('Language detected: ' + lang)

    const standalone = navigator.standalone
    console.log('standalone: ' + standalone)

    // const userAgent = window.navigator.userAgent
    const iOSVersion = checkVersion()
    // console.log('user agent: ' + userAgent)

    return {
      storedMotivationData: motivationJson,
      storedSessionSettings: sessionSettingsJson,
      storedUpdatingState : updatingStateJson,
      storedShowWakelockHint : showWakelockHintJson,
      safeAreaInsets : sa,
      width : width,
      height : height,
      browserLang : lang, 
      standalone : standalone,
      iOSVersion : iOSVersion
    }
  }


export const onReady = ({app, env}) => {
    if (app.ports && app.ports.outgoing) {
        app.ports.outgoing.subscribe(({tag, data}) => {
            switch (tag) {
                case 'STORE_MOTIVATION_DATA':
                    console.log('Saving data to localStorage: ' + data)
                    localStorage.setItem(MOTIVATION_DATA_KEY, JSON.stringify(data))
                    return

                case 'STORE_SESSION_SETTINGS':
                    localStorage.setItem(SESSION_SETTINGS_KEY, JSON.stringify(data))
                    return

                case 'SET_UPDATING':
                    localStorage.setItem(UPDATING_KEY, JSON.stringify(data))
                    return

                case 'STORE_SHOW_WAKELOCK_NOTE':
                    localStorage.setItem(SHOWWAKELOCKNOTE_KEY, JSON.stringify(data))
                    return

                case 'GET_SAFE_AREA':
                    app.ports.safeAreaReceiver.send({
                          sat: getComputedStyle(document.documentElement).getPropertyValue("--sat")
                          , sab: getComputedStyle(document.documentElement).getPropertyValue("--sab")
                          , sal: getComputedStyle(document.documentElement).getPropertyValue("--sal")
                          , sar: getComputedStyle(document.documentElement).getPropertyValue("--sar")
                        })
                    return


                case 'PLAY_SOUND':
                    console.log('Playing sound ' + data)

                   
                    var audio = new Howl({src: [data]})
                    audio.play()
                    return


                case 'SET_WAKE_LOCK':
                    console.log('WakeLock request received from Elm')


                    const requestWakeLock = async () => {
                        try {
                            wakeLock = await navigator.wakeLock.request('screen');

                            wakeLock.addEventListener('release', () => {
                                console.log('Wake Lock was released');
                            });
                            console.log('Wake Lock is active');
                        }
                        catch(err) {
                            console.error(`${err.name}, ${err.message}`);
                        }
                    };
                    requestWakeLock()
                    return
                

                case 'RELEASE_WAKE_LOCK':
                    console.log('Wakelock release requested by Elm')
                    if (wakeLock !== null) {
                        wakeLock.release().then(() => {
                            wakeLock = null;
                        })
                    }
                    return


                case 'CLIPBOARD_WRITE':
                    navigator.clipboard.writeText(data).then(
                        () =>{
                            console.log('Writing to clipboard successful.')
                        },
                        () => {
                            console.log('Writing to clipboard failed...')
                        },
                    );
                    return

                case 'REQUEST_CLIPBOARD':
                    navigator.clipboard.readText()
                        .then((clipText) => app.ports.clipboardReceiver.send(clipText))
                    return


                default:
                    console.warn(`Unhandled outgoing port: "${tag}"`)
                    return
            }
        })
    }
}