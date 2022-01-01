(ns advent-of-code.2016.day-02
  (:require [clojure.string :as str]))

;;;; --- Day 2: Bathroom Security ---
;;;; https://adventofcode.com/2016/day/2

(defn parse-input [s]
  (map seq (str/split-lines s)))

(def up [4 1, 5 2, 6 3, 7 4, 8 5, 9 6
         \3 \1, \6 \2, \7 \3, \8 \4, \A \6, \B \7, \C \8, \D \B])

(def right [1 2, 4 5, 7 8, 2 3, 5 6, 8 9
            \5 \6, \2 \3, \6 \7, \A \B, \3 \4, \7 \8, \B \C, \8 \9])

(defn next-button [b x]
  (case x
    \U ((apply hash-map up) b b)
    \R ((apply hash-map right) b b)
    \L ((apply hash-map (reverse right)) b b)
    \D ((apply hash-map (reverse up)) b b)))

(comment
    ;; part 1
  (let [input (parse-input (slurp "input/2016/02-codes.txt"))]
    (->> input
         (reductions (fn [b is] (reduce next-button b is)) 5)
         (drop 1)
         (apply str)))   ;=> "24862"

  ;; part 2
  (let [input (parse-input (slurp "input/2016/02-codes.txt"))]
    (->> input
         (reductions (fn [b is] (reduce next-button b is)) \5)
         (drop 1)
         (apply str)))   ;=> "24862"
  )