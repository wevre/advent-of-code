(ns advent-of-code.2023.day-05-seeds
  (:require [advent-of-code.common :as common]))

(defn parse-map [lines]
  (map common/parse-longs (drop 1 lines)))

(defn convert-with-range [[vs vl :as val] [dest src len]]
  (cond
    (<= (+ vs vl) src) [val]
    (< vs src) (if (<= (+ vs vl) (+ src len))
                 [[vs (- src vs)] (reduced [dest (- vl (- src vs))])]
                 [[vs (- src vs)] (reduced [dest len]) [(+ src len) (- (+ vs vl) (+ src len))]])
    (< vs (+ src len)) (if (<= (+ vs vl) (+ src len))
                         [(reduced [(+ dest (- vs src)) vl])]
                         [(reduced [(+ dest (- vs src)) (- (+ src len) vs)]) [(+ src len) (- (+ vs vl) (+ src len))]])
    :else [val]))

(defn convert-with-map [val ranges]
  (->> (reduce (fn [val's range] (mapcat #(if (reduced? %) [%] (convert-with-range % range)) val's))
               [val]
               ranges)
       (map unreduced)))

(defn convert [val maps]
  (reduce (fn [val's map] (mapcat #(convert-with-map % map) val's))
          [val]
          maps))

(comment
  (def input (slurp "input/2023/05-seeds.txt"))
  (def input (slurp "input/2023/05-sample-seeds.txt"))

  (convert-with-range [53 1] [49 53 8])
  (convert-with-map [53 1] [[49 53 8] [0 11 42]])

  ;; year 2023 day 05 puzzle 1
  (let [[seeds & maps] (common/split-grouped-lines input)
        seeds (common/parse-longs (first seeds))
        maps (map parse-map maps)]
    (->> (mapcat #(convert % maps) (for [v seeds] [v 1]))
         (map first)
         (apply min)))   ;; => 51580674

  ;; year 2023 day 05 puzzle 2
  (let [[seeds & maps] (common/split-grouped-lines input)
        seeds (common/parse-longs (first seeds))
        maps (map parse-map maps)
        seeds (partition 2 seeds)]
    (->> (mapcat #(convert % maps) seeds)
         (map first)
         (apply min)))   ;; => 99751240
  )
