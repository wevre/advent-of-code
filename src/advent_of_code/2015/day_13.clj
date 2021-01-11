(ns advent-of-code.2015.day-13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

;; --- Day 13: Knights of the Dinner Table ---

(defn parse [s]
  (let [[_ p1 op amt p2] (re-matches #"(\S+).*(gain|lose) (\d+) .* (\S+)\." s)]
    {(hash-set p1 p2) (({"gain" + "lose" -} op) (edn/read-string amt))}))

(defn hap-map [input] (apply merge-with + (map parse (str/split-lines input))))

(defn happiness [hap-map pp]
  (->> (cycle pp)
       (partition 2 1)
       (take (count pp))
       (map (comp hap-map set))
       (reduce +)))

(defn optimal-happiness [people hap-map]
  (reduce max (map #(happiness hap-map %) (combo/permutations people))))

(defn puzzle1 [input]
  (let [hap-map (hap-map input)
        people (reduce set/union #{} (keys hap-map))]
    (optimal-happiness people hap-map)))

(comment
  (let [input (slurp "input/2015/13-seating.txt")]
    (puzzle1 input)))

(defn puzzle2 [input]
  (let [hap-map (hap-map input)
        people (reduce set/union #{} (keys hap-map))
        hap-map (into hap-map (map #(vector #{"me" %} 0) people))
        people (conj people "me")]
    (optimal-happiness people hap-map)))

(comment
  (let [input (slurp "input/2015/13-seating.txt")]
    (puzzle2 input)))
