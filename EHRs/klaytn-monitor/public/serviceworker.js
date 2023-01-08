const CACHE_NAME = "klaytnwatchcache-1";
const urlsToCache = [ 'index.html' ];
// const urlsToCache = [ 'index.html', 'offline.html' ];

const self = this;

// Install SW
self.addEventListener("install", (event) => {
    event.waitUntil(
        caches.open(CACHE_NAME)
        .then(cache => {
            console.log("opened Cache")
            return cache.addAll(urlsToCache);
        })
    )
})

// Listen for request
self.addEventListener("fetch", (event) => {
    event.respondWith(
        caches.match(event.request)
        .then( () => {
            return fetch(event.request)
                    .catch( () => caches.match('index.html'))
        })
    )
})

// Activate SW
self.addEventListener("activate", (event) => {
    const cacheWhitelist = [];
    cacheWhitelist.push(CACHE_NAME);

    event.waitUntil(
        caches.keys().then((cacheNames) => Promise.all(
            cacheNames.map((cacheName) => {
                if(!cacheWhitelist.includes(cacheName)){
                    return caches.delete(cacheName);
                }
            })
        ))
    )
})