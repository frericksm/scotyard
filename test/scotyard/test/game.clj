(ns scotyard.test.game
  (:use [scotyard.game])
  (:use [clojure.test]))

(def g {:round 1, 
        :detectives [{:position 141, :color :red, :moved? false} 
                     {:position 174, :color :green, :moved? false} 
                     {:position 91, :color :blue, :moved? false} 
                     {:position 198, :color :yellow, :moved? false} 
                     {:position 53, :color :black, :moved? false}], 
        :mrx {:color :mrx, 
              :moved? false, 
              :responded? true,
              :positions #{197 50 117 13 29 155 103 
                           34 112 138 132 26 94}}})

(deftest test-game
  (let [g2 (as-> g x 
             (scotyard.game/move-mrx x "U")
             (scotyard.game/move-detective x :red 133)
             (scotyard.game/mrx-responds x false)
             (scotyard.game/move-detective x :green 161)
             (scotyard.game/mrx-responds x false)
             )]
    (is (= 133 (get-in g2 [:detectives 0 :position])) 
        "Red should have moved to place 133")
    (is (= 161 (get-in g2 [:detectives 1 :position])) 
        "Green should have moved to place 161")
    (is (= 91 (get-in g2 [:detectives 2 :position])) 
        "Blue should not have moved from place 91")))
