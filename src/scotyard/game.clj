(ns scotyard.game
  (:require [scotyard.metric :as m]
            [scotyard.map :as map]
            [scotyard.core]
            [clojure.set :as set])
  )

(def colors [:red :green :blue :yellow :black :purple :orange])
(def game-ref (atom {:round 0
                     :detectives []
                     :mrx-cloud #{}}))

(defn make-detective [index position]
  {:position position            ;; the current position 
   :color    (nth colors index)  ;; to identify the agent by color
   :moved?   false               ;; to indicate whether this agent
                                 ;; already moved while a round        
   })             

(defn detective-positions [game]
  (as-> game x
    (:detectives x)
    (map :position x)
    (set x)))

(defn init-1 
  "Initialize a new game"
  [& positions-detectives]
  (let [start-positions-detectives (set positions-detectives)
        number-agents-not-ok?      (> (count positions-detectives) 6)
        not-allowed?               (empty? (clojure.set/intersection start-positions-detectives
                                                                     scotyard.map/start-cards ))]
    (cond number-agents-not-ok?  (throw (IllegalStateException. 
                                         "Too many detectives")) 
          not-allowed?           (throw (IllegalStateException. 
                                         "Detectives at illegal start positions"))
          true                   {:round     1
                                  :detectives  (->> (map-indexed make-detective 
                                                                 start-positions-detectives)
                                                    vec)
                                  :mrx-cloud   (set (remove start-positions-detectives 
                                                            start-cards))
                                  :mrx-moved? false ;; to indicate
                                                    ;; whether mrx
                                                    ;; already moved
                                                    ;; while a round  
                                  :mrx-responded? true ;; To indicate
                                                       ;; wether mrx
                                                       ;; responded to move of detective 
                                  })))

(defn init-2 
  "Initialize a new game"
  [nr-detectives]
  (let [positions-detectives (as-> scotyard.map/start-cards x 
                               (shuffle x) 
                               (take nr-detectives x)
                               (set x))]
    (apply init-1 positions-detectives)))

(defn mrx-to-move? [game]
  (not (:mrx-moved? game)))

(defn move-mrx [game ticket]
  (if (not (mrx-to-move? game))
    (throw (IllegalStateException. "It's not Mr. X turn"))
    (as-> game x 
      (update-in x [:mrx-cloud] 
                 (fn [mc] 
                   (let [new-mrx-cloud (scotyard.core/mrx-moves ticket (detective-positions game) mc)]
                     (if (empty? new-mrx-cloud)
                       (throw (IllegalStateException. "Illegal move"))
                       new-mrx-cloud))))
      (assoc x :mrx-moved? true))))

(defn which-detectives-turn [game]
  (if (not (mrx-to-move? game))
    (as-> (:detectives game) x
      (filter (fn [d] (not (:moved? d))) x)
      (first x))))

(defn find-detective [game detective-index-or-color]
  (if (keyword? detective-index-or-color)
    (as-> game x
      (:detectives x)
      (map-indexed (fn [i d] [i d]) x)
      (filter (fn [[i d]] (= detective-index-or-color (:color d))) x)
      (first x))
    detective-index-or-color))

(defn move-detective [game detective-index-or-color to]
  (let [[detective-index detective] (find-detective game detective-index-or-color) 
        detective-on-turn (which-detectives-turn game)
        not-on-turn?  (not= (:color detective) (:color detective-on-turn))
        to-reachable? (contains? (scotyard.map/neighbours-for-detective (:position detective))
                                 to)]
    (cond  (empty? detective-on-turn) (throw (RuntimeException. 
                               (format "Illegal move. It's Mr. X turn"))) 
           not-on-turn? (throw (RuntimeException. 
                               (format "Illegal move. It's not detecvice's %s turn" (:color detective) )))
          (not to-reachable?)  (throw (RuntimeException. 
                                       (format "Place %d is not reachable for detective %s from place %d" to detective (:position detective))))
          true  (as-> game x 
                  (assoc-in x [:detectives detective-index :position] to)
                  (assoc-in x [:detectives detective-index :moved?] true)))))

(defn mrx-arrested 
  ""
  [game arrested?]
  (let [dec-positions (detective-positions game)
        common-pos    (clojure.set/intersection dec-positions (:mrx-cloud game))
        in-touch?     (not (empty? common-pos))]
    (cond (and arrested? in-touch?) (assoc game :mrx-cloud common-pos)
          (not arrested?)           (update-in game [:mrx-cloud]  (fn [c] (set (remove common-pos c))))
          true (throw (IllegalStateException. "Unzulässige Operation")))))

(defn round-finished? [game]
  (and (:mrx-moved? game)
       (reduce (fn [a v] (and a (:moved?  v))) true (:detectives game))))




