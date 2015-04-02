(ns user
  (:require [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer (refresh)])
  (:use [scotyard.game]
        [scotyard.strategy]
        [scotyard.metric]
        [scotyard.map]))

(def system nil)

(defn init []
  (alter-var-root #'system (constantly (init-2 5))))
  

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system
    (fn [s] (when s (component/stop s)))))

(defn go []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
