(ns advent-of-code.2015.day-08
  (:require [clojure.string :as str]))

;;;; --- Day 8: Matchsticks ---
;;;; https://adventofcode.com/2015/day/8

(defn parse-input [s] (str/split-lines s))

(defn escape-count [s]
  (let [escapes (re-seq #"\\\"|\\\\|\\x[0-9a-f-A-F]{2}" s)]
    (- (count s)   ; from raw string length ...
       2           ; ... subtract two for outer quotes ...
                   ; ... remove all but one character of each escape sequences
       (->> escapes (map (comp dec count)) (reduce +)))))

(defn re-escape-count* [s]
  (let [escapes (re-seq #"\\|\"" s)]
    (+ (count s)           ; to raw string length ...
       2                   ; ... add 2 for new outer quotes ...
       (count escapes)))   ; ... add a 1 for every original \ or ".
  )

(defn solve [input counter]
  (->> input
       (map counter)
       (map #(apply - %))
       (reduce +)))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2015/8-strings.txt"))]
    (solve input (juxt count escape-count)))   ;=> 1350

  ;; part 2
  (let [input (parse-input (slurp "input/2015/8-strings.txt"))]
    (solve input (juxt re-escape-count* count)))   ;=> 2085
  )
