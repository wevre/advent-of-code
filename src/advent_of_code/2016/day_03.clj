(ns advent-of-code.2016.day-03
  (:require [advent-of-code.common :refer [parse-longs]]))

;;;; --- Day 3: Squares With Three Sides ---
;;;; https://adventofcode.com/2016/day/3

(defn parse-input [s]
  (partition 3 (parse-longs s)))

(defn valid-triangle? [[a b c]]
  (and (< a (+ b c))
       (< b (+ a c))
       (< c (+ a b))))

(defn column-triangles [input]
  (mapcat #(apply map list %) (partition 3 input)))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2016/03-triangles.txt"))]
    (->> input
         (filter valid-triangle?)
         count))   ;=> 982

  ;; part 2
  (let [input (parse-input (slurp "input/2016/03-triangles.txt"))]
    (->> input
         column-triangles
         (filter valid-triangle?)
         count))   ;=> 1826
  )
