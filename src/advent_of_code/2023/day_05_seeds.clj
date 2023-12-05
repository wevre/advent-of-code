(ns advent-of-code.2023.day-05-seeds
  (:require [advent-of-code.common :as common]))

(defn parse-map [lines]
  (map common/parse-longs (drop 1 lines)))

(defn box-valid-range
  ([s l] (box-valid-range identity s l))
  ([f s l] (some-> (when (pos? l) [s l]) f vector)))

(defn convert-with-range* [val [dst src len]]
  (if (reduced? val)
    [val]
    (let [[vs vl] val
          ve (+ vs vl)
          srce (+ src len)]
      (concat   ;; Compose the (valid) subranges of val...
       (box-valid-range vs (min vl (- src vs)))   ;; ..._left_ of src
       (let [is (max src vs) ie (min ve srce)]   ;; ..._inside_ of src
         (box-valid-range reduced (+ dst (- is src)) (min len (- ie is))))
       (let [i (max srce vs)] (box-valid-range i (- ve i)))))))   ;; ..._right_ of src

(defn convert-with-map [val ranges]
  (->> (reduce (fn [val's range] (mapcat #(convert-with-range* % range) val's))
               [val]
               ranges)
       (map unreduced)))

(defn convert [val maps]
  (reduce (fn [val's map] (mapcat #(convert-with-map % map) val's))
          [val]
          maps))

(defn parse-input [input]
  (let [[seeds & maps] (common/split-grouped-lines input)]
    {:seeds (common/parse-longs (first seeds))
     :maps (map parse-map maps)}))

(defn solve [maps seeds]
  (->> (mapcat #(convert % maps) seeds)
       (map first)
       (apply min)))

(comment
  (def input (slurp "input/2023/05-seeds.txt"))
  (def input (slurp "input/2023/05-sample-seeds.txt"))

  ;; year 2023 day 05 puzzle 1
  (time
   (let [{:keys [seeds maps]} (parse-input input)
         seeds (for [v seeds] [v 1])]
     (solve maps seeds)))   ;; => 51580674

  ;; year 2023 day 05 puzzle 2
  (time
   (let [{:keys [seeds maps]} (parse-input input)
         seeds (partition 2 seeds)]
     (solve maps seeds)))   ;; => 99751240
  )
