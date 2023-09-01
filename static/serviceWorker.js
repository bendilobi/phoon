// Increment version when you update any of the local resources, which will
// in turn trigger the install event again.
const PRECACHE = "precache-v0.3.9";

// A list of local resources we always want to be cached.
const PRECACHE_URLS = [ 
    "/",
    "/registerServiceWorker.js",
    "/index.html",
    "/howler.core.min.js",
    "/scroll.css",
    "/audio/ding.mp3",
    "/audio/sessionEnd.mp3",
    "/audio/retention.mp3",
    "/audio/relaxRetention.mp3",
    "/audio/breathing.mp3",
    "/img/logo/favicon.png",
    "/manifest.json",
    "/favicon.ico"
  ];

// The install handler takes care of precaching the resources we always need.
self.addEventListener("install", (event) => {
  event.waitUntil(
    caches
      .open(PRECACHE)
      .then((cache) => cache.addAll(PRECACHE_URLS))
      .then(self.skipWaiting())
  );
});

// The activate handler takes care of cleaning up old caches.
self.addEventListener("activate", (event) => {
  const currentCaches = [PRECACHE];
  event.waitUntil(
    caches
      .keys()
      .then((cacheNames) => {
        return cacheNames.filter(
          (cacheName) => !currentCaches.includes(cacheName)
        );
      })
      .then((cachesToDelete) => {
        return Promise.all(
          cachesToDelete.map((cacheToDelete) => {
            return caches.delete(cacheToDelete);
          })
        );
      })
      .then(() => self.clients.claim())
  );
});

self.addEventListener("fetch", (event) => {
  console.log(`URL requested: ${event.request.url}`);

  // TODO: Requests nach /assets hier in den cache aufnehmen?
  //       Scheint zu funktionieren. Aber macht es auch Sinn?
  //       Durch den obigen Listener für "activate" wird 
  //       der Cache bei der Aktivierung gelöscht...
  // if (event.request.url.includes("assets")) {
  //   event.waitUntil(caches
  //     .open("elm-cache")
  //     .then((cache) => cache.add(event.request.url))
  //   );
  // }

  // Skip cross-origin requests, like those for Google Analytics.
  if (event.request.url.startsWith(self.location.origin) 
      && !event.request.url.includes("serviceWorker.js")) {

      event.respondWith(
        caches.match(event.request).then(cachedResponse => {
            const networkFetch = fetch(event.request).then(response => {
              // update the cache with a clone of the network response
              const responseClone = response.clone()
              caches.open(PRECACHE).then(cache => {
                cache.put(event.request, responseClone)
              })
              return response
            }).catch(function (reason) {
              console.error('ServiceWorker fetch failed: ', reason)
            })
            // prioritize cached response over network
            return cachedResponse || networkFetch
          }
        )
      )



    // event.respondWith(
    //   fetch(event.request).catch(function () {
    //     return caches.match(event.request);
    //   })
    // );
  }
});