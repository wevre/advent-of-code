(ns advent-of-code.2021.day-02
  (:require [clojure.string :as str]))

;; --- Day 2: Dive! ---
;; https://adventofcode.com/2021/day/2

;; An approach that is more consistent for both parts, is to reduce the input
;; with a function that takes a vector of [hpos vpos] and returns an updated
;; vector based on the current command. For part two it's a vector of [pos depth
;; aim] and same approach.

(defn second-sums
  "Returns sum of second value in list of lists.
  input of [(up 3) (up 5)] returns sum of 8."
  [s]
  (->>
   s
   (map second)
   (map #(Integer/parseInt %))
   (apply +)))

(defn group-commands [input]
  (->>
   input
   (re-seq #"[\d\w]+")
   (partition 2)
   (group-by first)
   (reduce-kv (fn [m k v] (assoc m (keyword k) (second-sums v))) {})))

(defn puzzle [input]
  (let [{:keys [forward down up]} (group-commands input)]
    (* forward (- down up))))

(comment
  (puzzle (slurp "input/2021/2-dive.txt"))
  )

(defn parse [input]
  (->>
   input
   (re-seq #"[\d\w]+")
   (partition 2)
   (map (fn [[cmd val]] [(keyword cmd) (Integer/parseInt val)]))))

(defn position
  "Inputs here needs to be pairs of commands and values:
  [(:forward 3) (:up 7) ...]"
  [input]
  (loop [[i & is] (parse input) pos 0 depth 0 aim 0]
    (if (nil? i)
      (* pos depth)
      (let [[cmd val] i]
        (case cmd
          :down (recur is pos depth (+ aim val))
          :up (recur is pos depth (- aim val))
          :forward (recur is (+ pos val) (+ depth (* val aim)) aim))))))

(comment
  (position (slurp "input/2021/2-dive.txt")))