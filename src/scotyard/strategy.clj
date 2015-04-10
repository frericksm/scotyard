(ns scotyard.strategy
  (:require [scotyard.metric :as m]
            [scotyard.map :as map]
            [scotyard.game :as g]))

(defn create-alphabeta 
  ""
  [branch? children heuristic-fn next-step-fn]
  (letfn [(alphabeta [node depth alpha beta maximizing-player?]
            (if (or (= depth 0) (not (branch? node)))
              (heuristic-fn node)
              (if maximizing-player?
                (loop [v Long/MIN_VALUE
                       a alpha
                       cs (children node)
                       child-node nil]
                  (if (or (empty? cs) (<= beta a))
                    (do (next-step-fn child-node) v)
                    (let [child-v (alphabeta (first cs) (dec depth) a beta false)
                          v' (max v child-v)
                          a' (max a v')
                          child-node' (if (= a a') child-node (first cs))]
                      (recur v' a' (rest cs) child-node'))))
                (loop [v Long/MAX_VALUE
                       b beta
                       cs (children node)
                       child-node nil]
                  (if (or (empty? cs) (<= b alpha))
                    (do (next-step-fn child-node) v)
                    (let [child-v (alphabeta (first cs) (dec depth) alpha b true)
                          v' (min v child-v)
                          b' (min b v')
                          child-node' (if (= b b') child-node (first cs))]
                      (recur v' b' (rest cs) child-node')))))))]
    alphabeta))

(defn measure-vector-1
  "Returns a vector of some measures of a game"
  [game detective-id]
  (let [dm (m/distance-matrix game)]
    [(m/minmin dm)
     (m/minmax dm)
     (m/maxmin dm)
     (m/maxmax dm)
     (m/detectives-row-norm dm detective-id m/min-seminorm)
     (m/detectives-row-norm dm detective-id m/euclidic-norm)]))

(defn measure-vector-2 
  "Returns a vector of some measures of a game"
  [game]
  (let [dm (m/distance-matrix game)]
    [(m/minmin dm)
     (m/minmax dm)
     (m/maxmin dm)
     (m/maxmax dm)]))

(defn measure-vector-3
  "Returns a vector of some measures of a game"
  [game]
  (let [dm (m/distance-matrix game)]
    [ (->> game :mrx :positions count)
      (->> dm vals m/euclidic-norm)]))


(defn measure 
  "Calculates one number of a measure vector"
  [v]
  (reduce (fn [a k] (+ k (* 1000 a))) 0 v))


(defn heuristic-3 [g] (* -1.0 (measure (measure-vector-3 g))))


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
      (sort-by (fn [g] (measure (measure-vector-1 g (:color detective)))) x )
      (first x))))


(defn children-children 
  "Takes a game in which it is detective d's turn and returns a possible game states
  where the detective d has moved"
  [game]
  (let [[i detective] (g/find-detective game (g/whos-turn? game))]
    (as-> (map/neighbours-for-detective (:position detective)) x
      (clojure.set/difference x (g/detective-positions game))
      (map (fn [to] (g/move-detective game (:color detective) to)) x)
      (map (fn [g] (g/mrx-responds g false)) x))))

(defn children
  "Calculates a list of games from 'game'. If it is Mr. X turn than a list 
  of size 1 is returned containing the game after Mr. X travelled by 
  black ticket ('X'). Otherwise a list of games is returned containing all 
  possible states after each detective has moved"
  [game]
  (let [next (g/whos-turn? game)]
    (if (= :mrx  (g/whos-turn? game))
      ;; Mr X. moves
      (as-> game x
        (g/move-mrx x "X") ;; His best choice X = Black ticket
        (vector x))
      ;; All possible detectives' moves in this round
      (as->  game  x
        ;; tree of all intermediate steps
        (tree-seq (fn [g] (= (:round g) (:round game))) children-children x) 
        ;; Only states of the game of the next round 
        (filter (fn [g] (not= (:round g) (:round game))) x) 
        ))))

(defn branch? 
  "Takes a game and return true if Mr. X has not to reveal"
  [game]
  (not (g/mrx-has-to-reveal? game)))

(defn next-steps-all-detectives
  "Take a game and a depth and returns the minimaxed next game state" 
  ([g depth]
   (let [next-state (atom nil)
         next-step-fn (fn [g] (swap! next-state (fn [_] g)))
         ab (create-alphabeta branch? children heuristic-3 next-step-fn)
         maximize? (not= :mrx (g/whos-turn? g))]
     (ab g depth Long/MIN_VALUE Long/MAX_VALUE maximize?)
     (deref next-state)))
  ([g]
   (next-steps-all-detectives g 1)))
