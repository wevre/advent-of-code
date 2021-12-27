(ns advent-of-code.2015.day-05
  (:require [clojure.string :as str]))

;;;; --- Day 5: Doesn't He Have Intern-Elves For This? ---
;;;; https://adventofcode.com/2015/day/5

(defn nice? [s]
  (and
   (<= 3 (count (re-seq #"[aeiou]" s)))   ; at least three vowels
   (re-find #"([a-z])\1" s)               ; two letters in a row
   (not (re-find #"ab|cd|pq|xy" s))))     ; no naughty strings

(defn nice-v2? [s]
  (and
   (re-find #"([a-z][a-z]).*\1" s)   ; non-overlap repeating pair
   (re-find #"([a-z])[a-z]\1" s)))   ; repeat with letter in-between

(comment
  ;; part 1
  (->> (slurp "input/2015/5-naughty_or_nice.txt")
       (str/split-lines)
       (filter nice?)
       count)   ;=> 236

  ;; part 2
  (->> (slurp "input/2015/5-naughty_or_nice.txt")
       (str/split-lines)
       (filter nice-v2?)
       count)   ;=> 51
)
