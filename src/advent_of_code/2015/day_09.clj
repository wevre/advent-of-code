(ns advent-of-code.2015.day-09
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

;;;; --- Day 9: All in a Single Night ---
;;;; https://adventofcode.com/2015/day/9

(defn parse-route [s]
  (let [[_ from to dist] (re-matches #"(\S+) to (\S+) = (\d+)" s)]
    [#{from to} (edn/read-string dist)]))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-route)
       (into {})))

(defn route-dist [graph route]
  (->> (partition 2 1 route)
       (map (comp graph set))
       (reduce +)))

(defn solve [rf input]
  (let [cities (reduce set/union (keys input))]
    (->> (combo/permutations cities)
         (map (partial route-dist input))
         (reduce rf))))

(comment
  ;; part 1 -- 450ms
  (time
   (let [input (parse-input (slurp "input/2015/9-cities.txt"))]
     (solve min input)))   ;=> 207

  ;; part 2 -- 440ms
  (time
   (let [input (parse-input (slurp "input/2015/9-cities.txt"))]
     (solve max input)))   ;=> 804
)
