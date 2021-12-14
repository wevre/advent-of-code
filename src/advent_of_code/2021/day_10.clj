(ns advent-of-code.2021.day-10
  (:require [clojure.string :as str]))

;; --- Day 10: Syntax Scoring ---
;; https://adventofcode.com/2021/day/10

(def input (->> (slurp "input/2021/10-syntax.txt")
                (str/split-lines)))

(def matches {\< \>, \[ \], \{ \}, \( \)})

(defn scan
  "Return vector of [nil `stack`] if line is incomplete, where `stack` is list
   of characters needed to complete. If line is invalid, return vector is
   [`c` `stack`] where `c` is the invalid character and `stack` can be ignored."
  [line]
  (loop [stack () [c & cs] line]
    (cond
      (matches c) (recur (conj stack (matches c)) cs)
      (= c (first stack)) (recur (next stack) cs)
      :else [c stack])))

(comment
  ;; puzzle 1
  (->> input
       (map scan)
       (keep first)
       (map {\) 3, \] 57, \} 1197, \> 25137})
       (reduce +)))

(defn auto-comp-score [cs]
  (->> (map {\) 1, \] 2, \} 3, \> 4} cs)
       (reduce (fn [acc val] (+ val (* acc 5))) 0)))

(comment
  ;; puzzle 2
  (let [scores (->> input
                    (map scan)
                    (remove first)
                    (map second)
                    (map auto-comp-score)
                    sort)]
    (nth scores (quot (count scores) 2)))
  )