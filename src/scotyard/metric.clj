(ns scotyard.metric
  (:require [scotyard.astar :as astar]
            [scotyard.map :as map]))

(defn vector-add [v1 v2]
  (->> (map (fn [a b] (+ a b)) v1 v2)
       (vec)))

(defn vector-scalar-mult [v1 s]
  (->> (map (fn [a] (* a s)) v1)
       (vec)))

(defn euclidic-norm [v]
  (->> (map (fn [a] (* a a)) v)
       (apply +)
       (Math/sqrt)))

(defn euclidic-dist [v1 v2]
  (->> (vector-scalar-mult v2 -1)
       (vector-add v1)
       (euclidic-norm)))

(def ^{:doc "Enthält den maximalen euklidischen Abstand zweier Nachbarfelder"}
  max-dist-neighbours
  (->> map/symap
       (keys)
       (map seq)
       (map #(euclidic-dist (get map/sypos (first %)) (get map/sypos (second %))))
       (apply max)))

(defn heuristic
  "Schätzt die Kosten, um von from nach to zu gelangen. Die Schätzung beruht auf dem euklidischen Abstand der beiden Felder.
  Damit die Kosten niemals überschätzt werden, wird der euklidische Abstand durch max-dist-neighbours dividiert.
  Damit ist diese heuristic monoton im Sinne des A*-Algorithmus"
  [from to]
  (/ (euclidic-dist (get map/sypos from) (get map/sypos to)) max-dist-neighbours))

(defn cost
  "Die Kosten von einem Knoten zu seinem Nachbarknoten"
  [from to]
  (if (= from to) 0 1))

(defn search
  "Liefert den kürzesten Weg von 'from' nach 'to'"
  [from to]
  (astar/astar from to map/neighbours-for-detective heuristic cost ))

(def ^{:doc "memoized Version der Funktion search"}
  m-search (memoize search))

(defn distance
  "Die Kosten zwischen 2 beliebigen Knoten"
  [a b]
  (- (count (m-search a b)) 1))



(defn distance-matrix 
  "Returns all distances between the detectives and the possible locations of Mr. X as a map. 
  Key of a map entry is a vector [color mrx-position] and the value is the distance between the 
  position of the <color> detective and the possible position <mrx> of Mr. X."
  [game]
  (as-> game x
    (for [d (:detectives x) mrx (get-in x [:mrx :positions])]
      [[(:color d) mrx]  (distance (:position d) mrx)])
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








