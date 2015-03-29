(ns scotyard.map
    (:require [clojure.java.io :as io]))

;;http://scotland-yard.cvs.sourceforge.net/viewvc/scotland-yard/scotland-yard/ScotlandYard/bin/SCOTMAP.TXT?revision=1.1.1.1
;;hat Fehler zum Feld 199
(def symap (->> "resources/map.txt"
                (io/reader)
                (line-seq)
                (drop 2)
                (map #(vec (.split % " ")))
                (reduce (fn [a [von nach mit]]
                          (let [key #{(Integer/parseInt von) 
                                      (Integer/parseInt nach)}]
                            (assoc a key (conj (get a key) mit )))) 
                        {} )))

(def start-cards #{13,26,29,34,50,53,91,94,103,112,117,132,138,141,155,174,197,198})


(def black-ticket-edges {#{108 115} (list "X")
                         #{115 157} (list "X")
                         #{157 194} (list "X")
                         })

(def symap-mrx (merge symap black-ticket-edges))

(def underground-stations #{1 13 46 67 74 79 89 93 111 128 140 153 163 185})

;;"http://scotland-yard.cvs.sourceforge.net/viewvc/scotland-yard/scotland-yard/ScotlandYard/bin/SCOTPOS.txt?revision=1.1.1.1"
(def sypos (->> "resources/pos.txt"                
                (io/reader)
                (line-seq)
                (drop 2)
                (map #(vec (.split % " ")))
                (reduce (fn [a [feld x y] ]
                          (assoc a (Integer/parseInt feld)
                                 [(Integer/parseInt x) (Integer/parseInt y)]))
                        {})))

(defn neighbours-for-detective [node]
  (disj (->> symap
             (keys)
             (filter #(contains? % node))
             (reduce (fn [a s] (into a s)) #{}))
        node ))

(defn neighbours-for-mrx [node transport]
  (disj (->> symap-mrx
             (keys)
             (filter #(contains? % node))
             (filter #(or (= "X" transport) 
                          (contains? (set (get symap-mrx %)) transport)))
             (reduce (fn [a s] (into a s)) #{}))
        node))
