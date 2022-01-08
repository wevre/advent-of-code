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
      (* speed (+ (* q fly) (min r fly))))))

(def time-limit 2503)

(defn winners [reindeer]
  (fn [seconds]
    (let [flown (group-by (distance seconds) reindeer)]
      (get flown (reduce max (keys flown))))))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2015/14-reindeer.txt"))]
    (->> input (map (distance time-limit)) (reduce max)))   ;=> 2696

  ;; part 2
  (let [input (parse-input (slurp "input/2015/14-reindeer.txt"))]
    (->> (take time-limit (iterate inc 1))
         (mapcat (winners input))   ; Find winners at each second.
         frequencies
         vals
         (reduce max)))   ;=> 1084
  )
