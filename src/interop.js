export const onReady = ({app, env}) => {
    if (app.ports && app.ports.outgoing) {
        app.ports.outgoing.subscribe(({tag, data}) => {
            switch (tag) {
                case 'PLAY_SOUND':
                    console.log('PLaying sound...')
                    var audio = new Audio('/audio/bell.mp3')
                    audio.play()
                    return
                default:
                    console.warn('Unhandled outgoing port: "${tag}"')
            }
        })
    }
}