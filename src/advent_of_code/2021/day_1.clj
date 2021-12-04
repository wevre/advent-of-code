(ns advent-of-code.2021.day-1
  (:require [clojure.string :as str]))

;; --- Day 1: Sonar Sweep ---

(defn get-input [file]
  (->>
   (slurp file)
   (str/split-lines)
   (map #(Integer/parseInt %))))

(defn puzzle1
  "Returns count of increasing pairs."
  [depths]
  (->>
   (partition 2 1 depths)
   (filter #(apply < %))
   count))

(defn puzzle2
  "Returns count of increasing triple-sums."
  [depths]
  (->>
   (partition 3 1 depths)
   (map #(apply + %))
   puzzle1))

(comment
  (puzzle1 (get-input "input/2021/1-depths.txt"))
  (puzzle2 (get-input "input/2021/1-depths.txt")))
