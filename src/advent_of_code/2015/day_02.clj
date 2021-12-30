(ns advent-of-code.2015.day-02
  (:require [clojure.string :as str]
            [advent-of-code.common :refer [parse-longs]]))

;;;; --- Day 2: I Was Told There Would Be No Math ---
;;;; https://adventofcode.com/2015/day/2

(defn parse-input [s]
  (for [l (str/split-lines s)] (sort (parse-longs l))))

(defn calc-wrapper [[x y z]] (+ (* 3 x y) (* 2 x z) (* 2 y z)))

(defn calc-ribbon [[x y z]] (+ x x y y (* x y z)))

(comment
  ;; part 1
  (->> (parse-input (slurp "input/2015/2-presents.txt"))
       (map calc-wrapper)
       (reduce +))  ;=> 1588178

  ;; part 2
  (->> (parse-input (slurp "input/2015/2-presents.txt"))
       (map calc-ribbon)
       (reduce +))   ;=> 3783758
)
