// Increment version when you update any of the local resources, which will
// in turn trigger the install event again.
const PRECACHE = "precache-v0.8.16";
const ELMCACHE = "compiledElm";

// A list of local resources we always want to be cached.
const PRECACHE_URLS = [ 
    // "/",
    "/registerServiceWorker.js",
    "/howler.core.min.js",
    "/app.css",
    "/audio/ding.mp3",
    "/audio/sessionEnd.mp3",
    "/audio/retention.mp3",
    "/audio/relaxRetention.mp3",
    "/audio/breathing.mp3",
    "/audio/inhale.mp3",
    "/audio/exhale.mp3",
    "/audio/breathingEnd.mp3",
    "/img/logo/favicon.png",
    "/img/logo/favicon-192x192.png",
    "/manifest.json",
    "/favicon.ico",
    "/img/bmac/bmac_de.png",
    "/img/bmac/bmac_en.png"
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
  const currentCaches = [PRECACHE, ELMCACHE];
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

  if (event.request.url.startsWith(self.location.origin) 
    && !event.request.url.includes("serviceWorker.js") 
    && !event.request.url.includes("version.json")) {

    event.respondWith(
      caches.match(event.request).then((cachedResponse) => {
        if (cachedResponse !== undefined) {

          // Make sure that newer versions of index.html are cached and
          // therefore used if they are fetched next time
          if (event.request.url.includes("/#/optimize")) {
            fetch(event.request).then((response) => {
              const responseClone = response.clone();
              caches.open(PRECACHE).then((cache) => {
                cache.put(event.request, responseClone);
              });
            });
          }

          return cachedResponse;

        } else {
          // We only fetch stuff from the server if it isn't already
          // in our PRECACHE (except the index.html, see above)
          return fetch(event.request).then((response) => {
            const responseClone = response.clone();

            // "assets" contains our compiled elm code whose name
            // contains a hash and thus changes for each new version.
            // To avoid filling the cache with old versions, we have 
            // a dedicated cache for the compiled elm which we empty
            // whenever a new version is fetched
            if (event.request.url.includes("assets")) {
              caches.delete(ELMCACHE).then(() => {
                caches.open(ELMCACHE).then((cache) => {
                  cache.put(event.request, responseClone);
                });
              });

            // All other requests are added to the precache 
            } else {
              caches.open(PRECACHE).then((cache) => {
                cache.put(event.request, responseClone);
              });
            }
            return response;
          });
        }
      })
    )
  }
});