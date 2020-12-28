(ns advent-of-code.2015.day-9
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set]))

;; parse the input and generate a set of all the cities
;; also generate a map of sets of city pairs (because we don't care about order)
;; use combinatorics to determine all possible combinations of all cities
;; partition those into pairs, turn them into sets, and look up the distances
;; then add up distances for each combo and find the smallest.

(defn parse [s]
  (let [[_ from to dist] (re-find #"(\S+) to (\S+) = (\d+)" s)]
    {#{from to} (Integer/parseInt dist)}))

(defn route-dist [graph route]
  (->> (partition 2 1 route)
       (map (comp graph set))
       (reduce +)))

(defn all-dists [in]
  (let [graph (into {} (map parse (str/split-lines in)))
        cities (reduce clojure.set/union (keys graph))]
    (map (partial route-dist graph) (combo/permutations cities))))

(defn puzzle1 [in]
  (reduce min (all-dists in)))

(defn puzzle2 [in]
  (reduce max (all-dists in)))

(comment
  (let [input "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141"] (puzzle1 input))
  
  (let [input (slurp "input/2015/9-cities.txt")] (puzzle1 input))
  
  (let [input (slurp "input/2015/9-cities.txt")] (puzzle2 input)))