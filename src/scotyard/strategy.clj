(ns scotyard.strategy
  (:require [scotyard.metric :as m]
            [scotyard.map :as map]
            [clojure.set :as set]))

(defn distance-matrix 
  "Returns all distances between the detectives and the possible locations of Mr. X as a map. 
  Key of a map entry is a vector [color mrx-position] and the value is the distance between the 
  position of the <color> detective and the possible position <mrx> of Mr. X."
  [game]
  (as-> game x
    (for [d (:detectives x) mrx (get-in x [:mrx :positions])]
      [[(:color d) mrx]  (m/distance (:position d) mrx)])
    (into {} x)))

(defn min-seminorm 
  "Returns the minimum of the absolute (real valued) entries of v"
  [v]
  (as-> v x
    (map (fn [z] (Math/abs z)) x)
    (reduce min x)))

(defn max-norm [v]
  "Returns the maximum of the absolute (real valued) entries of v"
  (as-> v x
    (map (fn [z] (Math/abs z)) x)
    (reduce max x)))

(defn dimension-reduction  
  "Takes a map as returned by the function distance-matrix and returns a map where only the 
  the dimenstion at 'dim-index' is kept. Values to the same dimension value are aggregated by 
  applying the 'norm-fn'"
  [distance-matrix norm-fn dim-index]
  (as-> distance-matrix x
    (reduce (fn [a [dims distance]] 
              (let [dim (nth dims dim-index)]
                (update-in a [dim] conj distance)))
            {} x)
    (map (fn [[k v]] [k (norm-fn v)]) x)
    (into {} x)))

(defn min-projection 
  "Calls dimension-reduction with 'min-seminorm' as norm function and dim-index 1 (which is the 
  dimension of Mr. X's locations)"
  [distance-matrix]
  (dimension-reduction distance-matrix min-seminorm 1))

(defn max-projection 
  "Calls dimension-reduction with 'max-norm' as norm function and dim-index 1 (which is the 
  dimension of Mr. X's locations)"
  [distance-matrix]
  (dimension-reduction distance-matrix max-norm 1))

(defn minmin 
  "Applies min-seminorm to the result of min-projection"
  [distance-matrix]
  (as-> distance-matrix x
    (min-projection x)
    (vals x)
    (min-seminorm x)))

(defn minmax 
  "Applies max-norm to the result of min-projection"
  [distance-matrix]
  (as-> distance-matrix x
    (min-projection x)
    (vals x)
    (max-norm x)))

(defn maxmin 
  "Applies min-seminorm to the result of max-projection"
  [distance-matrix]
  (as-> distance-matrix x
    (max-projection x)
    (vals x)
    (min-seminorm x)))

(defn maxmax 
  "Applies max-norm to the result of max-projection"
  [distance-matrix]
  (as-> distance-matrix x
    (max-projection x)
    (vals x)
    (max-norm x)))

(defn detectives-row-norm 
  "Select the row of detective-id and applies the norm-fn to that row"
  [distance-matrix detective-id norm-fn]
  (as-> distance-matrix x
    (filter (fn [[[color v]]] (= color detective-id)) x)
    (dimension-reduction x norm-fn 0)
    (vals x)
    (first x)))

(defn measure-vector 
  "Returns a vector of some measures of a game"
  [game detective-id]
  (let [dm (distance-matrix game)]
    [(minmin dm)
     (minmax dm)
     (maxmin dm)
     (maxmax dm)
     (detectives-row-norm dm detective-id min-seminorm)
     (detectives-row-norm dm detective-id m/euclidic-norm)]))

(defn measure 
  "Calculates one number of a measure vector"
  [v]
  (reduce (fn [a k] (+ k (* 1000 a))) 0 v))

(defn sort-games 
  "Sorts a coll of games according to the measure-vector"
  [games detective-id]
  (sort-by (fn [g] (measure (measure-vector g detective-id))) games))
