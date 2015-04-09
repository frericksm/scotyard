(ns scotyard.strategy
  (:require [scotyard.metric :as m]
            [scotyard.map :as map]
            [scotyard.game :as g]))


(defn measure-vector-1 
  "Returns a vector of some measures of a game"
  [game]
  (let [dm (m/distance-matrix game)]
    [(m/minmin dm)
     (m/minmax dm)
     (m/maxmin dm)
     (m/maxmax dm)]))

(defn measure-vector-2 
  "Returns a vector of some measures of a game"
  [game detective-id]
  (let [dm (m/distance-matrix game)]
    [(m/minmin dm)
     (m/minmax dm)
     (m/maxmin dm)
     (m/maxmax dm)
     (m/detectives-row-norm dm detective-id m/min-seminorm)
     (m/detectives-row-norm dm detective-id m/euclidic-norm)]))

(defn measure 
  "Calculates one number of a measure vector"
  [v]
  (reduce (fn [a k] (+ k (* 1000 a))) 0 v))

(defn next-detective-moves 
  "Simple strategy. Calculates a set of of new games by moving the detective who is turn to
  all directly reachable locations. Select that game with the measure-vector that has the lowest
  measure"
  [game]
  (let [next (g/whos-turn? game)
        [i detective] (g/find-detective game next)]
    (as-> (map/neighbours-for-detective (:position detective)) x
      (clojure.set/difference x (g/detective-positions game))
      (map (fn [to] (g/move-detective game (:color detective) to)) x)
      (sort-by (fn [g] (measure (measure-vector-2 g (:color detective)))) x )
      (first x))))

(defn create-branch-fn [from-game]
  (let [whosturn (g/whos-turn? from-game)
        max-round (inc (:round from-game))]
    (fn [game]
      (and (not (g/mrx-has-to-reveal? game))
           (or (not= (:round game) max-round)
               (not= (g/whos-turn? game) whosturn))))))

(defn branch? [game]
    (< (:round game) 2))


  
(defn children 
  "Takes a game and returns "
  [game]
  (let [next (g/whos-turn? game)]
    (if (= :mrx  next)
      (as-> game x
        (g/move-mrx x "X")
        (vector x))
      (let [[i detective] (g/find-detective game next)]
        (as-> (map/neighbours-for-detective (:position detective)) x
          (clojure.set/difference x (g/detective-positions game))
          (map (fn [to] (g/move-detective game (:color detective) to)) x)
          (map (fn [g] (g/mrx-responds g false)) x))))))


(defn next-steps-all-detectives [g]
  (let [branch? (create-branch-fn g)]
    (as-> (tree-seq branch? children g) x
      (filter (fn [gg] (not (branch? gg))) x)
      (sort-by (fn [gg] (measure (measure-vector-1 gg))) x)
      (first x))))
