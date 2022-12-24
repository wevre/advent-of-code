(ns advent-of-code.2022.day-18-droplets
  (:require [advent-of-code.common :as common]))

(defn add [& ps] (apply mapv + ps))

(def ∆3d [[0 0 -1] [0 0 +1]
          [0 -1 0] [0 +1 0]
          [-1 0 0] [+1 0 0]])

(defn parse [input]
  (zipmap (->> input common/parse-longs (partition 3)) (repeat :lava)))

(defn neighbors [c cubes]
  (->> (map #(add c %) ∆3d)
       (keep #(when (cubes %) %))))

(defn open-faces [cubes]
  (->> cubes
       (map #(neighbors (key %) cubes))
       (map #(- 6 (count %)))
       (apply +)))

(comment
  ;; puzzle 1
  (let [cubes (parse #_"2,2,2,1,2,2,3,2,2,2,1,2,2,3,2,2,2,1,2,2,3,2,2,4,2,2,6,1,2,5,3,2,5,2,1,5,2,3,5"
               (slurp "input/2022/18-droplets.txt"))]
    (open-faces cubes))   ; => 3412
  )

;; NOTE: my input cubes range from [0 0 0] to [18 19 19], so an envelope from
;; [-1 -1 -1] to [19 20 20] completely encases.

(defn flood [cubes p]
  (loop [n 0 frontier (into clojure.lang.PersistentQueue/EMPTY [p]) cubes cubes]
    (if-let [p (peek frontier)]
      (if (#{:air} (cubes p))
        (let [cubes (update cubes p #({:air :steam} % %))]
          (recur (inc n) (into (pop frontier) (->> (neighbors p cubes) (filter #(#{:air} (cubes %))))) cubes))
        (recur (inc n) (pop frontier) cubes))
      cubes)))

(comment
  (let [cubes (parse #_"2,2,2,1,2,2,3,2,2,2,1,2,2,3,2,2,2,1,2,2,3,2,2,4,2,2,6,1,2,5,3,2,5,2,1,5,2,3,5"
               (slurp "input/2022/18-droplets.txt"))
        world (into {} (for [x (range -1 20) y (range -1 21) z (range -1 21)
                             :let [k [x y z]]]
                         [k (if (cubes k) :lava :air)]))
        flooded (flood world [-1 -1 -1])
        air (into {} (filter #(= :air (val %)) flooded))]
    (- (open-faces cubes) (open-faces air)))   ; => 2018
  )
