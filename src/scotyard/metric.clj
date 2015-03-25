(ns scotyard.metric
  (:require [scotyard.astar :as astar]
            [scotyard.map :as map]))

(defn euclidic-dist [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- y2 y1) 2)
                (Math/pow (- x2 x1) 2))))

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

(defn distances-each-to-each [acoll bcoll]
  (for [a acoll b bcoll] (distance a b)))

(defn distances-to-each
  "Liefert einen Vector, der für jeden Punkt in coll die Distanz von a enthält"
  [a coll]
  (apply min (map #(distance a %) coll)))

(defn next-in-coll
  "Liefert die Elemente aus coll, die den geringsten Abstand zu a haben"
  [a coll]
  (let [min-dist (distances-to-each a coll)]
    (filter #(= min-dist (distance a %)) coll)))

(defn distance-seq
  "Returns the distances of each element of acoll to bcoll"
  [norm-fn acoll bcoll]
  (norm-fn (map #(distances-to-each % bcoll) acoll)))

(defn norm-sum
  [distance-seq]
  (apply + distance-seq))

(defn norm-nearer-is-important
  [distance-seq]
  (->> distance-seq
       (map #(Math/log10 (+ 1 %)))
       (norm-sum)
       ))

(defn nearer?
  "Returns true, if a1coll is nearer to bcoll than a2coll to bcoll"
  [a1coll a2coll bcoll]
  (let [dist-fn (partial distance-seq norm-sum)]
    (< (dist-fn a1coll bcoll)
       (dist-fn a2coll bcoll))))
