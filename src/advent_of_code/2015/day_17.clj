(ns advent-of-code.2015.day-17
  (:require [advent-of-code.common :refer [parse-longs]]
            [clojure.math.combinatorics :as combo]))

;;;; --- Day 17: No Such Thing as Too Much ---
;;;; https://adventofcode.com/2015/day/17

(defn containers [vol input]
  (->> input
       (map-indexed vector)
       (combo/subsets)
       (filter #(= vol (apply + (map second %))))))

(comment
  ;; part 1
  (let [input (parse-longs (slurp "input/2015/17-containers.txt"))]
    (count (containers 150 input)))   ;=> 1638

  ;; part 2
  (let [input (parse-longs (slurp "input/2015/17-containers.txt"))]
    (->> (containers 150 input)
         (group-by count)
         sort
         first
         second
         count))   ;=> 17
  )
