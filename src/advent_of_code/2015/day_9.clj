(ns advent-of-code.2015.day-9
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

;; --- Day 9: All in a Single Night ---

(defn parse [s]
  (let [[_ from to dist] (re-matches #"(\S+) to (\S+) = (\d+)" s)]
    {#{from to} (edn/read-string dist)}))

(defn route-dist [graph route]
  (->> (partition 2 1 route)
       (map (comp graph set))
       (reduce +)))

(defn puzzle [rf input] 
  (let [graph (into {} (map parse) (str/split-lines input))]
    (->> (reduce set/union (keys graph))
         combo/permutations
         (map (partial route-dist graph))
         (reduce rf))))

(comment
  (let [input (slurp "input/2015/9-cities.txt")] (puzzle min input))

  (let [input (slurp "input/2015/9-cities.txt")] (puzzle max input))

  (let [input "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141"] (puzzle min input)))