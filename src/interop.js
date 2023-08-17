var audio = new Audio('/audio/ding.mp3')

export const onReady = ({app, env}) => {
    if (app.ports && app.ports.outgoing) {
        app.ports.outgoing.subscribe(({tag, data}) => {
            switch (tag) {
                case 'PLAY_SOUND':
                    console.log('PLaying sound...')
                    
                    audio.pause()
                    audio.currentTime = 0
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