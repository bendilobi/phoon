
const MOTIVATION_DATA_KEY = 'motivationData'
const SESSION_SETTINGS_KEY = 'sessionSettings'
const UPDATING_KEY = 'updating'

export const flags = ({ env }) => {
    const motivationStored = localStorage.getItem(MOTIVATION_DATA_KEY)
    const motivationJson = motivationStored ? JSON.parse(motivationStored) : null

    const sessionSettingsStored = localStorage.getItem(SESSION_SETTINGS_KEY)
    const sessionSettingsJson = sessionSettingsStored ? JSON.parse(sessionSettingsStored) : null

    const updatingStateStored = localStorage.getItem(UPDATING_KEY)
    const updatingStateJson = updatingStateStored ? JSON.parse(updatingStateStored) : null 
    // TODO: Macht nicht arg viel Sinn hier, weils im Portrait Mode "0px" ist...
    const sal = getComputedStyle(document.documentElement).getPropertyValue("--sal")

    // console.log("sal: " + sal)

    return {
      storedMotivationData: motivationJson,
      storedSessionSettings: sessionSettingsJson,
      storedUpdatingState : updatingStateJson,
      safeAreaInsetLeft: sal
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

                case 'GET_SAFE_AREA':
                    app.ports.safeAreaReceiver.send(getComputedStyle(document.documentElement).getPropertyValue("--sal"))
                    return


                case 'PLAY_SOUND':
                    console.log('Playing sound ' + data)

                   
                    var audio = new Howl({src: [data]})
                    audio.play()
                    return


                case 'SET_WAKE_LOCK':
                    console.log('WakeLock request received from Elm')

                    let wakeLock = null;

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



                default:
                    console.warn(`Unhandled outgoing port: "${tag}"`)
                    return
            }
        })
    }
}