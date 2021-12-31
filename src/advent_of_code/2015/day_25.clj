(ns advent-of-code.2015.day-25)

;;;; --- Day 25: Let It Snow ---
;;;; https://adventofcode.com/2015/day/25

(def col 3075)
(def row 2981)

(defn s [n] (/ (* n (inc n)) 2))

(defn nth-code [r c] (- (s (+ r (dec c))) (dec r)))

(comment
  ;; part 1
  (->> (iterate #(rem (* % 252533) 33554393) 20151125)
       (take (nth-code row col))
       last)   ;=> 9132360
  )
