(ns advent-of-code.2022.day-18-droplets
  (:require [advent-of-code.common :as common]))

(defn add [& ps] (apply mapv + ps))

(def ∆3d [[0 0 -1] [0 0 +1] [0 -1 0] [0 +1 0] [-1 0 0] [+1 0 0]])

(defn parse [input]
  (zipmap (->> input common/parse-longs (partition 3)) (repeat :lava)))

(defn nighs [c cubes]
  (->> (map #(add c %) ∆3d) (keep #(when (cubes %) %))))

(defn open-faces [cubes]
  (->> cubes (map #(nighs (key %) cubes)) (map #(- 6 (count %))) (apply +)))

(def sample "2,2,2,1,2,2,3,2,2,2,1,2,2,3,2,2,2,1,2,2,3,2,2,4,2,2,6,1,2,5,3,2,5,2,1,5,2,3,5")

(comment
  ;; puzzle 1
  (let [lava (parse #_sample (slurp "input/2022/18-droplets.txt"))]
    (open-faces lava))   ; => 3412
  )

;; NOTE: My input cubes range from [0 0 0] to [18 19 19], so an envelope from
;; [-1 -1 -1] to [19 20 20] completely encases.

(defn flood [p cubes]
  (loop [frontier (into clojure.lang.PersistentQueue/EMPTY [p]) cubes cubes]
    (if-let [p (peek frontier)]
      ;; If position `p` is :air enqueue its neighbors and change it to :steam.
      (recur (cond-> (pop frontier) (#{:air} (cubes p)) (into (nighs p cubes)))
             (update cubes p #({:air :steam} % %)))
      cubes)))

(defn build-world [ux uy uz]
  (zipmap
   (for [x (range -1 ux) y (range -1 uy) z (range -1 uz)] [x y z])
   (repeat :air)))

(comment
  (let [lava (parse #_sample (slurp "input/2022/18-droplets.txt"))
        air (->> (merge (build-world 20 21 21) lava)
                 (flood [-1 -1 -1])
                 (filter #(= :air (val %)))
                 (into {}))]
    (- (open-faces lava) (open-faces air)))   ; => 2018
  )
