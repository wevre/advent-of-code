(ns advent-of-code.2015.day-5
  (:require [clojure.string :as str]))

;; --- Day 5: Doesn't He Have Intern-Elves For This? ---

(defn nice? [s]
  (and
   (<= 3 (count (re-seq #"[aeiou]" s)))   ; at least three vowels
   (re-find #"([a-z])\1" s)               ; two letters in a row
   (not (re-find #"ab|cd|pq|xy" s))))     ; no naughty strings

(defn nice-v2? [s]
  (and
   (re-find #"([a-z][a-z]).*\1" s)   ; non-overlap repeating pair
   (re-find #"([a-z])[a-z]\1" s)))   ; repeat with letter in-between

(defn puzzle [pred input]
  (->> (str/split-lines input)
       (filter pred)
       count))

(comment
  (let [input (slurp "input/2015/5-naughty_or_nice.txt")] (puzzle nice? input))

  (let [input "ugknbfddgicrmopn
aaa
jchzalrnumimnmhp
haegwjzuvuyypxyu
dvszwmarrgswjxmb"] (puzzle nice? input))

  (let [input (slurp "input/2015/5-naughty_or_nice.txt")]
    (puzzle nice-v2? input))

  (let [input "qjhvhtzxzqqjkmpb
xxyxx
uurcxstgmygtbstg
ieodomkazucvgmuy"] (puzzle nice-v2? input)))
