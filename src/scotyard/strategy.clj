(ns scotyard.strategy
  (:require [scotyard.metric :as m]
            [scotyard.map :as map]
            [clojure.set :as set]))


(defn distance-matrix [game]
  (as-> game x
    (for [d (:detectives x) mrx (get-in x [:mrx :positions])]
      [#{(:color d) mrx}  (m/distance (:position d) mrx)])
    (into {} x)))


(defn messzahlen [game detective-id]
  (let [distance-matrix (distance-matrix game)]))
