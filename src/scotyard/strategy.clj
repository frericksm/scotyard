(ns scotyard.strategy
  (:require [scotyard.metric :as m]
            [scotyard.map :as map]
            [clojure.set :as set]))

(defn distance-matrix [game]
  (as-> game x
    (for [d (:detectives x) mrx (get-in x [:mrx :positions])]
      [[(:color d) mrx]  (m/distance (:position d) mrx)])
    (into {} x)))

(defn min-norm [v]
  (reduce (fn [a b] (min a b)) Integer/MAX_VALUE  v))

(defn max-norm [v]
  (reduce (fn [a b] (max a b)) 0 v))

(defn norm-projection [distance-matrix norm-fn dim-index]
  (as-> distance-matrix x
    (reduce (fn [a [dims distance]] 
              (let [dim (nth dims dim-index)]
                (update-in a [dim] conj distance)))
            {} x)
    (vals x)
    (map norm-fn x)))

(defn min-projection [distance-matrix]
  (norm-projection distance-matrix min-norm 1))

(defn max-projection [distance-matrix]
  (norm-projection distance-matrix max-norm 1))

(defn minmin [distance-matrix]
  (as-> distance-matrix x
    (min-projection x)
    (min-norm x)))

(defn minmax [distance-matrix]
  (as-> distance-matrix x
    (min-projection x)
    (max-norm x)))

(defn maxmin [distance-matrix]
  (as-> distance-matrix x
    (max-projection x)
    (min-norm x)))

(defn maxmax [distance-matrix]
  (as-> distance-matrix x
    (max-projection x)
    (max-norm x)))

(defn messzahlen [game detective-id]
  (let [dm (distance-matrix game)]
    [(minmin dm)
     (minmax dm)
     (maxmin dm)
     (maxmax dm)
     (as-> dm x
       (filter (fn [[[color v]]] (= color detective-id)) x)
       (norm-projection x min-norm 0)
       (first x))
     (as-> dm x
       (filter (fn [[[color v]]] (= color detective-id)) x)
       (norm-projection x m/euclidic-norm 0)
       (first x))
     ]))
