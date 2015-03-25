(ns scotyard.astar
  ;;(:use [scotyard.priority-map :only [priority-map]])
  )

(defn neighbours-g-scores [current neighbours cost-fn g-score-map]
  (reduce (fn [a n]
            (assoc a n
                   (+ (get g-score-map current)
                      (cost-fn current n))))
          {}
          neighbours))

(defn filter-neighbours-g-scores [neighbours-g-scores open g-score-map]
  (into {}
        (filter
         (fn [[node g-score]]
           (or (not (contains? (into #{} open) node))
               (< g-score (get g-score-map node))))
         neighbours-g-scores))
  )

(defn neighbours-f-scores [heuristic-fn neigh-g-scores goal]
  (reduce (fn [a [node g-score]]
            (assoc a node
                   (+ g-score
                      (heuristic-fn node goal))))
          {}
          neigh-g-scores))

(defn reconstruct-path [goal came-from]
  (reverse (take-while identity  (iterate came-from goal))) )

(defn astar
  [start goal neighbours-fn heuristic-fn cost-fn & {mono :monotonic :or {mono true}}]
  (loop [g-score-map {start (cost-fn start start)}
         f-score-map (neighbours-f-scores heuristic-fn g-score-map goal)  ;;{start (+ (get g-score-map start) (heuristic-fn start goal))}
         open        (list start)
         closed      #{}
         came-from   {}]
    (when-let [current (first open)]      
      (if (= goal current)
        (reconstruct-path goal came-from)
        (do ;(println current  open closed)
          (let [neighbours (remove closed (neighbours-fn current))
                neighbours-g-scores (neighbours-g-scores current neighbours 
                                                         cost-fn g-score-map)
                neighbours-g-scores-filtered (filter-neighbours-g-scores
                                              neighbours-g-scores open g-score-map) 
                neighbours-f-scores-filtered (neighbours-f-scores
                                              heuristic-fn
                                              neighbours-g-scores-filtered
                                              goal)
                new-g-score-map (merge g-score-map neighbours-g-scores-filtered)
                new-f-score-map (merge f-score-map neighbours-f-scores-filtered)
                new-open (sort-by new-f-score-map
                                  (concat (rest open) 
                                          (keys neighbours-g-scores-filtered)))
                new-closed (if mono (conj closed current) closed)
                new-came-from (reduce (fn [a n] (assoc a n current))
                                      came-from
                                      (keys neighbours-g-scores-filtered))]            
            (recur new-g-score-map
                   new-f-score-map
                   new-open 
                   new-closed 
                   new-came-from)
            ))))))

