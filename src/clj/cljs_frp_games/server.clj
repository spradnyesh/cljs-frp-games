(ns cljs-frp-games.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response])
  (:gen-class))

(defn handler [request]
  (cond (= "/" (:uri request))
        (response/redirect "/templates/index.html")
        (= "/game-of-life" (:uri request))
        (response/redirect "/templates/game-of-life.html")
        :else (response/redirect "/templates/index.html")))

(def app 
  (-> handler
      (resources/wrap-resource "public")))

(defn -main [& args]
  (jetty/run-jetty app {:port 3000}))

