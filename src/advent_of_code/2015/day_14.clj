(ns advent-of-code.2015.day-14
  (:require [clojure.string :as str]
            [advent-of-code.common :refer [parse-longs]]))

;;;; --- Day 14: Reindeer Olympics ---
;;;; https://adventofcode.com/2015/day/14

(defn parse-input [s]
  (for [line (str/split-lines s)] (parse-longs line)))

(defn distance [seconds]
  (fn [[speed fly rest]]
    (let [[q r] ((juxt quot rem) seconds (+ fly rest))]
      (* speed (+ (* fly q) (min r fly))))))

(def time-limit 2503)

(defn winners [reindeer]
  (fn [seconds]
    (let [flown (group-by (distance seconds) reindeer)]
      (get flown (apply max (keys flown))))))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2015/14-reindeer.txt"))]
    (->> input (map (distance time-limit)) (reduce max)))   ;=> 2696

  ;; part 2
  (let [input (parse-input (slurp "input/2015/14-reindeer.txt"))]
    (->> (range)
         (drop 1)
         (take time-limit)
         (mapcat (winners input))
         frequencies
         vals
         (reduce max)))   ;=> 1084
  )
