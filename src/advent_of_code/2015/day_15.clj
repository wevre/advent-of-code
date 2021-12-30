(ns advent-of-code.2015.day-15
  (:require [clojure.string :as str]
            [advent-of-code.common :refer [parse-longs range-inc]]))

;;;; --- Day 15: Science for Hungry People ---
;;;; https://adventofcode.com/2015/day/15

(defn parse-input [s]
  (apply mapv vector (map parse-longs (str/split-lines s))))

(defn find-max [tsps pred props]
  (let [scores (for [i1 (range-inc 1 (- tsps 3))
                     i2 (range-inc 1 (- tsps i1))
                     i3 (range-inc 1 (- tsps i1 i2))
                     :let [i4 (- tsps i1 i2 i3)
                           weighted (map #(reduce + (map * % [i1 i2 i3 i4])) props)]
                     :when (pred (last weighted))]
                 (->> weighted
                      butlast
                      (map #(max 0 %))
                      (reduce *)))]
    (reduce max scores)))

(comment
  ;; part 1
  (let [props (parse-input (slurp "input/2015/15-ingredients.txt"))]
    (find-max 100 (constantly true) props))   ;=> 21367368

  ;; part 2
  (let [props (parse-input (slurp "input/2015/15-ingredients.txt"))]
    (find-max 100 #(= 500 %) props))   ;=> 1766400
  )
