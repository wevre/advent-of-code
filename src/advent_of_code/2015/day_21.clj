(ns advent-of-code.2015.day-21
  (:require [clojure.java.math :as math]))

;;;; --- Day 21: RPG Simulator 20XX ---
;;;; https://adventofcode.com/2015/day/21

(def boss {:hit-points 104
           :damage 8
           :armor 1})

(def weapons       [{:cost 8   :damage 4 :armor 0}
                    {:cost 10  :damage 5 :armor 0}
                    {:cost 25  :damage 6 :armor 0}
                    {:cost 40  :damage 7 :armor 0}
                    {:cost 74  :damage 8 :armor 0}])

(def armor         [{:cost 0   :damage 0 :armor 0}
                    {:cost 13  :damage 0 :armor 1}
                    {:cost 31  :damage 0 :armor 2}
                    {:cost 53  :damage 0 :armor 3}
                    {:cost 75  :damage 0 :armor 4}
                    {:cost 102 :damage 0 :armor 5}])

(def rings-damage  [{:cost 0   :damage 0 :armor 0}
                    {:cost 25  :damage 1 :armor 0}
                    {:cost 50  :damage 2 :armor 0}
                    {:cost 100 :damage 3 :armor 0}])

(def rings-defense [{:cost 0   :damage 0 :armor 0}
                    {:cost 20  :damage 0 :armor 1}
                    {:cost 40  :damage 0 :armor 2}
                    {:cost 80  :damage 0 :armor 3}])

(defn wins? [player enemy]
  (let [enemy-attack (max 1 (- (:damage enemy) (:armor player)))
        player-attack (max 1 (- (:damage player) (:armor enemy)))
        turns (int (math/ceil (/ (:hit-points enemy) player-attack)))]
    (< (* (dec turns) enemy-attack) (:hit-points player))))

(defn trials [hit-points]
  (for [w weapons
        a armor
        g rings-damage
        f rings-defense
        :let [items (merge-with + {:hit-points hit-points} w a g f)]]
    (assoc items :win? (wins? items boss))))

(comment
  ;; part 1
  (->> (trials 100)
       (filter :win?)
       (sort-by :cost)
       first
       #_:cost)   ;=> 78

  ;; part 2
  (->> (trials 100)
       (remove :win?)
       (sort-by :cost >)
       first
       #_:cost)   ;=> 148
  )
