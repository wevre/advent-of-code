(ns advent-of-code.2015.day-13
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

;;;; --- Day 13: Knights of the Dinner Table ---
;;;; https://adventofcode.com/2015/day/13

(defn parse-hap [s]
  (let [[_ p1 op amt p2] (re-matches #"(\S+).*(gain|lose) (\d+) .* (\S+)\." s)]
    {(hash-set p1 p2) (({"gain" + "lose" -} op) (edn/read-string amt))}))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map parse-hap)
       (apply merge-with +)))

(defn happiness [hap-map]
  (fn [pp]
    (->> (cycle pp)
         (partition 2 1)
         (take (count pp))
         (map (comp hap-map set))
         (reduce +))))

(defn optimal-happiness [hap-map]
  (let [people (reduce set/union #{} (keys hap-map))]
    (->> (combo/permutations people)
         (map (happiness hap-map))
         (reduce max))))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2015/13-seating.txt"))]
    (optimal-happiness input))   ;=> 709

  ;; part 2
  (let [input (parse-input (slurp "input/2015/13-seating.txt"))
        people (reduce set/union #{} (keys input))
        input (into input (map #(vector #{"me" %} 0) people))]
    (optimal-happiness input))   ;=> 668
  )
