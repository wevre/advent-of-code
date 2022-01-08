(ns advent-of-code.2015.day-24-v2
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code.common :refer [parse-longs]]))

;;;; --- Day 24: It Hangs in the Balance ---
;;;; https://adventofcode.com/2015/day/24

;; This code finds the correct solution, but it stops as soon as it finds a
;; valid combination, without checking if the remaining numbers _also_ form
;; valid combinations.

(defn valid-combos [xs targ]
  (fn [n]
    (->> (combo/combinations xs n)
         (filter #(= targ (reduce + %))))))

(defn min-entangled [xs targ]
  (->> (iterate inc 1)
       (map (valid-combos xs targ))
       (filter not-empty)
       first
       (map (partial reduce *))
       (reduce min)))

(comment
  ;; part 1 -- 606ms
  (time
   (let [input (parse-longs (slurp "input/2015/24-packages.txt"))
         targ (/ (reduce + input) 3)]
     (min-entangled input targ)))   ;=> 10723906903

  ;; part 2 -- 26ms
  (time
   (let [input (parse-longs (slurp "input/2015/24-packages.txt"))
         targ (/ (reduce + input) 4)]
     (min-entangled input targ)))   ;=> 74850409
  )
