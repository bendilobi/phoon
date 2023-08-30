
// const MOTIVATION_DATA_KEY = 'motivationData'

export const flags = ({ env }) => {
    const motivationStored = localStorage.getItem('motivationData')
    const motivationJson = motivationStored ? JSON.parse(motivationStored) : null
    return {
      storedMotivationData: motivationJson
    }
  }


export const onReady = ({app, env}) => {
    if (app.ports && app.ports.outgoing) {
        app.ports.outgoing.subscribe(({tag, data}) => {
            switch (tag) {
                case 'RELOAD_APP':
                    window.location.reload();
                    return


                // case 'STORE_DATA':
                //     console.log('Saving data to localStorage:' + data)
                //     localStorage.setItem('data', JSON.stringify(data))
                //     return
                case 'STORE_MOTIVATION_DATA':
                    console.log('Saving data to localStorage: ' + data)
                    localStorage.setItem('motivationData', JSON.stringify(data))
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