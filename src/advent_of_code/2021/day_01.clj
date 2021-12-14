(ns advent-of-code.2021.day-01
  (:require [advent-of-code.common :refer [parse-longs]]))

;; --- Day 1: Sonar Sweep ---
;; https://adventofcode.com/2021/day/1

(defn count-incr-pairs
  "Return count of increasing pairs."
  [depths]
  (->>
   (partition 2 1 depths)
   (filter #(apply < %))
   count))

(defn sum-of-triples
  "Return sums of triplets."
  [depths]
  (->>
   (partition 3 1 depths)
   (map #(apply + %))))

(comment
  ;; puzzle 1
  (count-incr-pairs (parse-longs (slurp "input/2021/1-depths.txt")))

  ;; puzzle 2
  (count-incr-pairs (sum-of-triples (parse-longs (slurp "input/2021/1-depths.txt"))))
  )
