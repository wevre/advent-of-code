(ns advent-of-code.2015.day-17
  (:require [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]))

;; --- Day 17: No Such Thing as Too Much ---

(defn containers [vol input]
  (->> (map edn/read-string (re-seq #"\d+" input))
       (map-indexed vector)
       (combo/subsets)
       (filter #(= vol (apply + (map second %))))))

(defn puzzle1 [vol input]
  (count (containers vol input)))

(comment
  (let [input (slurp "input/2015/17-containers.txt")] (puzzle1 150 input)))

(defn puzzle2 [vol input]
  (count (second (first (sort (group-by count (containers vol input)))))))

(comment
  (time (let [input (slurp "input/2015/17-containers.txt")] 
          (puzzle2 150 input))))
