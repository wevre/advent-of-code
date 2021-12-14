(ns advent-of-code.2021.day-06
  (:require [advent-of-code.common :refer [parse-longs]]))

;; --- Day 6: Lanternfish ---
;; https://adventofcode.com/2021/day/6

(def get-count
  "Return count of fish after `gen` generations, based on the starting `age`."
  (memoize
   (fn [gen age]
     (if (> gen age)
       (+ (get-count (- gen age 1) 6) (get-count (- gen age 1) 8))
       1))))

(defn fish-count [gens input]
  (reduce + (map #(get-count gens %) input)))

(comment
  ;; puzzle 1
  (fish-count 80 (-> (slurp "input/2021/6-fish.txt") (parse-longs)))

  ;; puzzle 2
  (fish-count 256 (-> (slurp "input/2021/6-fish.txt") (parse-longs)))
  )