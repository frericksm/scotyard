(ns scotyard.game
  (:require [scotyard.map :as map]
            [clojure.set :as set]))

;; Colors of the detectives. Kind of an identifier
(def colors [:red :green :blue :yellow :black :purple :orange])

(def colors_union_mr_x [:mrx :red :green :blue :yellow :black :purple :orange])

;; After moving in this round Mr. X has to reveal his current position
(def reveal-in-round #{3 8 13 18})

(defn make-detective 
  "Returns a map representing the value of a detective"
  [index position]
  {;; the current position 
   :position position            

   ;; to identify the agent by color
   :color (nth colors index)  

   ;; to indicate whether this agent already moved while the current round        
   :moved? false})

(defn detective-positions [game]
  (as-> game x
    (:detectives x)
    (map :position x)
    (set x)))

(defn init-1 
  "Initialize a new game"
  [& positions-detectives]
  ;; Checks
  (if (not= (count positions-detectives) (count (set positions-detectives)))
    (throw (IllegalStateException. (format "More than one detective at one place"))))
  (if (> (count positions-detectives) 6)
    (throw (IllegalStateException. "Too many detectives")))
  (if (not= (count (set positions-detectives))
            (count (clojure.set/intersection (set positions-detectives)
                                             scotyard.map/start-cards )))
    (throw (IllegalStateException. (format "Detectives at illegal start positions: %s"
                                           (clojure.set/difference (set positions-detectives)
                                                                   scotyard.map/start-cards)))))
  ;; Action
  (let [start-positions-detectives (set positions-detectives)]
    {:round     1
     :detectives  (->> start-positions-detectives (map-indexed make-detective) vec)
     :mrx {
           ;; Mr. X has 'color' :rmx
           :color :mrx
           
           ;; Mr. X's possible locations when the game starts
           :positions (set (remove start-positions-detectives 
                                   scotyard.map/start-cards))

           ;; to indicate if Mr. X already moved while a round  
           :moved? false 

           ;; to indicate if Mr. X responded to move of detective 
           :responded? true}}))

(defn init-2 
  "Initialize a new game"
  [nr-detectives]
  (let [positions-detectives (as-> scotyard.map/start-cards x 
                               (shuffle x) 
                               (take nr-detectives x)
                               (set x))]
    (apply init-1 positions-detectives)))

(defn round-finished? 
  "Returns true, if Mr. X and all detectives have moved in the current round."
  [game]
  (and (->> game :mrx :moved?)
       (reduce (fn [a v] (and a (:moved? v))) true (:detectives game))))

(defn whos-turn? 
  "Returns keyword :mrx if it is Mr.X turn. Returns the color keyword of the next detective who is turn."
  [g]
  ;(if (round-finished? g) (throw (IllegalStateException. "Round finished")))
  (if (not (->> g :mrx :moved?)) 
    :mrx
    (as-> (:detectives g) x
      (cons (:mrx g) x)
      (filter (fn [d] (not (:moved? d))) x)
      (map :color x)
      (first x))))

(defn mrx-moves 
  "Takes a transport flag (T = Taxi, B = Bus, U = Underground, X = Black ticket), the current positions 
  of the detectives and the possible locations of Mr. X and returns the possible new locations of Mr. X"
  [transport current-detectives-pos possible-mrx-positions ]
  (let [cleaned-mrx-ps (clojure.set/difference (set possible-mrx-positions)
                                               (set current-detectives-pos))]
    (->> cleaned-mrx-ps
         (reduce (fn [a n] (into a (scotyard.map/neighbours-for-mrx n transport))) #{})
         (remove (set current-detectives-pos))
         (set))))

(defn find-detective 
  "Takes a game and index/color of a detective and return a map representing the detective"
  [game detective-index-or-color]
  (if (keyword? detective-index-or-color)
    (as-> game x
      (:detectives x)
      (map-indexed (fn [i d] [i d]) x)
      (filter (fn [[i d]] (= detective-index-or-color (:color d))) x)
      (first x))
    [detective-index-or-color (nth (:detectives game) 
                                   detective-index-or-color )]))

(defn mrx-has-to-reveal? 
  "Takes a game and returns true if Mr. X has to reveal his current position"
  [game]
  (and (contains? reveal-in-round (:round game))
       (->> game :mrx :moved?)
       (not= 1 (->> game :mrx :positions count))))

(defn check-mrx-revealed 
  "Takes a game and throws an IllegalStateException if Mr. X has to reveal his current position"
  [game]
  (if (mrx-has-to-reveal? game)
    (throw (IllegalStateException. 
            (str "First Mr. X has to reveal his current location")))))

(defn check-mrx-responded 
  "Takes a game and throws an IllegalStateException if Mr. X has not yet answered after a move of a detective.
  Mr. X answers by calling the function mrx-responds"
  [game]
  (if (not (->> game :mrx :responded?))
    (throw (IllegalStateException. 
            (str "First Mr. X has to respond to the last "
                 "detective's move by calling function 'mrx-responds'")))))

(defn check-turn 
  "Takes a game and color (an element of the set colors_union_mr_x) and throws a IllegalStateException if 
  the it is not that detective's turn (color :mrx means Mr. X)"
  [game color]
  (let [name (if (= color :mrx) "Mr. X" (str "Detective" (name color)))]
        (if (not= color (whos-turn? game))
          (throw (IllegalStateException. (format "It's not %s's turn" color))))))

(defn check-reachable 
  "Takes a game and the locations 'from' and 'to' and throws an IllegalArgumentException if there is no direct 
  connection between those two locations"
  [game from to]
  (if (not (contains? (scotyard.map/neighbours-for-detective from) to))
    (throw (IllegalArgumentException. 
            (format "Place %d is not reachable from place %d" 
                    to from)))))

(defn check-mrx-transport [game ticket]
  (let [new-positions (mrx-moves ticket 
                                 (detective-positions game) 
                                 (get-in game [:mrx :positions]))]
    (if (empty? new-positions)
      (throw (IllegalArgumentException. "Illegal move")))))

(defn check-mrx-arrested [game]
  (let [safe-mr-positions (clojure.set/difference 
                           (get-in game [:mrx :positions])
                           (detective-positions game))]
    (if (empty? safe-mr-positions)
      (throw (IllegalArgumentException. "Game over. Mr. X is caught by one detective")))))

(defn move-mrx 
  "Returns a new (value of the) game reflecting Mr. X moving around using the 'ticket'"
  [game ticket]

  ;; Checks
  (check-mrx-arrested game)
  (check-mrx-responded game)
  (check-turn game :mrx)
  (check-mrx-transport game ticket)
  
  ;; Action
  (as-> game x 
    (update-in x [:mrx :positions] 
               (fn [positions] 
                 (mrx-moves ticket 
                            (detective-positions game) 
                            positions)))
    (assoc-in x [:mrx :moved?] true)
    (assoc-in x [:mrx :responded?] true)))

(defn move-detective 
  ""
  [game detective-index-or-color to]
  (let [[detective-index detective] (find-detective game detective-index-or-color) 
        to-reachable? (contains? (scotyard.map/neighbours-for-detective (:position detective))
                                 to)]
    ;; Checks
    (check-mrx-arrested game)
    (check-mrx-revealed game)
    (check-mrx-responded game)
    (check-turn game (:color detective))
    (check-reachable game (:position detective) to)
    
    ;; Action
    (as-> game x 
      (assoc-in x [:detectives detective-index :position] to)
      (assoc-in x [:detectives detective-index :moved?] true)
      (assoc-in x [:mrx :responded?] false))))

(defn finish-round 
  [game]
  
;; Checks
  (check-mrx-arrested game)
  (if-let [next-player (whos-turn? game)]
    (throw (IllegalStateException. 
            (format "Round cannot be finished since %s has to move" next-player))))
  (check-mrx-responded game)

  ;; Actions
  (as-> game x
    (update-in x [:detectives] (fn [detectives]
                                 (->> detectives
                                      (map (fn [d] (assoc d :moved? false)))
                                      vec)))
    (assoc-in x [:mrx :moved?] false)
    (update-in x [:round] inc)))
                         

(defn mrx-responds 
  "Must be called after each move of a detective. If 'arrested?' is true, then the game is finished. If 'arrested?' is false then no agent is at the location of Mr. X. Returns a new value of the game."
  [game arrested?]
  (let [common-pos (clojure.set/intersection (detective-positions game)
                                             (->> game :mrx :positions))
        in-touch?  (not (empty? common-pos))]

    ;; Checks
    (if (and  arrested? (not in-touch?)) 
      (throw (IllegalArgumentException. 
              (str "Cannot be arrested since no detective "
                   "is located on a place where Mr. X may lingering around")))
      
    ;; Actions
    (as-> game x
      (cond 
        (and arrested? in-touch?) (assoc-in x [:mrx :positions] common-pos)
        (not arrested?) (update-in x [:mrx :positions]  
                                   (fn [c] (clojure.set/difference 
                                            c 
                                            (detective-positions game)))))

      (assoc-in x [:mrx :responded?] true)
      (if (round-finished? x)
        (finish-round x)
        x)))))




(defn mrx-reveals 
  "Mr. X annouces his current position. He is only supposed to do after moving in rounds 3,8,13 and 18.
  Return a new value of game narrowing the possible positions of Mr. X to the one element coll containing 
  position 'here-i-am'"
  [game here-i-am]
  
  ;; Checks
  (if (not (mrx-has-to-reveal? game))  
    (throw (IllegalStateException. 
            (format "Mr. X has only to reveal his own position after moving in rounds: %s" 
                    reveal-in-round))))
  (check-mrx-responded game)
  (if (not (contains? (->> game :mrx :positions) here-i-am))  
    (throw (IllegalStateException. 
            (format "Mr. X cannot be located at place %s since he has to be at one of the locations %s" 
                    here-i-am (->> game :mrx :positions)))))
    
  ;; Action
  (as-> game x 
    (assoc-in x [:mrx :positions]  #{here-i-am})))


