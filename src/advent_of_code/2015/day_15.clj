(ns advent-of-code.2015.day-15
  (:require [clojure.string :as str]
            [advent-of-code.common :refer [parse-longs range-inc]]))

;;;; --- Day 15: Science for Hungry People ---
;;;; https://adventofcode.com/2015/day/15

(defn parse-input [s]
  (apply mapv vector (map parse-longs (str/split-lines s))))

(defn weights [tsps props]
  (for [i (range-inc (- tsps 3))
        j (range-inc (- tsps i))
        k (range-inc (- tsps i j))
        :let [l (- tsps i j k)]
        :when (= tsps (+ i j k l))]
    (map #(reduce + (map * % [i j k l])) props)))

(defn score [weights]
  (->> weights
       butlast
       (map #(max 0 %))
       (reduce *)))

(comment
  ;; part 1 -- 737ms
  (time
   (let [props (parse-input (slurp "input/2015/15-ingredients.txt"))]
     (->> (weights 100 props)
          (map score)
          (reduce max))))   ;=> 21367368

  ;; part 2 -- 589ms
  (time
   (let [props (parse-input (slurp "input/2015/15-ingredients.txt"))]
     (->> (weights 100 props)
          (filter #(= 500 (last %)))
          (map score)
          (reduce max))))   ;=> 1766400
  )
