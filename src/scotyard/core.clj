(ns scotyard.core
  (:require [scotyard.metric :as m]
            [scotyard.map :as map]
            [clojure.set :as set])
  )

(defn possible-moves
  "Ermittelt alle Felder auf die ein Agent vom Feld 'agent-position' ziehen kann. 
  Die Felder 'all-agent-positions' auf denen andere Agenten stehen sind natürlich 
  nicht erreichbar"
  [all-agent-positions agent-position]
  (remove (set  all-agent-positions)
          (set (map/neighbours-for-detective agent-position)))
  )


(defn possible-new-ag-pos 
  "Liefert eine Coll von Vektoren mit Agentenpositionen. Dabei ist a ntln "
  [game current-agents]
  (let [current (deref game)
        current-agents (:agents (first current))
        current-mrx (:mrx (first current))
        next-agents (possible-moves nil current-agents)] 
    (reset! game (conj current {:mrx current-mrx
                                :agents next-agents}))
    (println "Nach:\n" next-agents)))

(defn mrx-moves [transport current-agent-pos possible-mrx-positions ]
  (let [cleaned-mrx-ps (remove (set current-agent-pos) (set possible-mrx-positions))]
    (->> cleaned-mrx-ps
         (reduce (fn [a n] (into a (map/neighbours-for-mrx n transport))) #{})
         (remove (set current-agent-pos)))))

(defn next-mrx-moves
  ([game transport]
   (let [current (deref game)
         current-agents (:agents (first current))
         current-mrx (:mrx (first current))
         next-mrx (mrx-moves transport current-agents current-mrx)]
     (println "Von:\n" current-mrx)
     
     (reset! game (conj current {:mrx next-mrx
                                 :agents current-agents}))
     (println "Nach:\n" next-mrx)))
  ([game position transport]
   (let [current (deref game)
         current-agents (:agents (first current))
         current-mrx (:mrx (first current)) 
         next-mrx (vector position)]
     (println "Von:\n" current-mrx)
     (reset! game (conj current {:mrx next-mrx
                                 :agents current-agents}))
     (println "Nach:\n" next-mrx))))

(comment

  (def game (atom {:agents [1 2 3 4 5]
                   :mrx [1 2 3 4 5]
                   }))

  (def pos-agents [1 2 3 4 5])
  
  (def pos-mrx [7 8 189 171])
  
  (def pos-mrx (mrx-moves "T" pos-agents pos-mrx))
  
  (def pos-agents (do (println pos-agents)
                      ((next-agent-positions pos-agents pos-mrx ))))
)
