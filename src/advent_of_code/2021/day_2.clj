(ns advent-of-code.2021.day-2
  (:require [clojure.string :as str]))

;; --- Day 2: Dive! ---

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

;; another way to do this would be to reduce into a map of :forward, :up, :down,
;; adding the newest amount to the value sitting under the appropriate key.

;; I like this solution here:
;; https://github.com/neilcode/advent-of-code-2021/blob/main/src/calendar/2021/day_02.clj
;; it uses condp which I've never used before.

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