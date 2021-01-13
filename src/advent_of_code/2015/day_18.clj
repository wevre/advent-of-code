(ns advent-of-code.2015.day-18
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;; --- Day 18: Like a GIF For Your Yard ---

(defn parse [lines]
  (reduce
   (fn [res [r l]] (into res (keep-indexed (fn [c v] (when (= \# v) [r c]))) l))
   #{}
   (map-indexed vector lines)))

(defn neighbors [cell]
  (->> (combo/selections [-1 0 1] (count cell))
       (remove #(apply = 0 %))
       (map #(map + cell %))))

(defn step [wid keep-on grid]
  (into keep-on
        (set (for [[[r c :as loc] n] (frequencies (mapcat neighbors grid))
                   :when (or (= 3 n) (and (grid loc) (= 2 n)))
                   :when (and (< -1 r wid) (< -1 c wid))]
               loc))))

(defn puzzle [cycles wid keep-on input]
  (->> (into keep-on (parse (str/split-lines input)))
       (iterate (partial step wid keep-on))
       (drop cycles)
       first
       count))

(comment
  (let [input (slurp "input/2015/18-lights.txt")
        wid 100
        cycles 100
        keep-on #_#{} #{[0 0] [0 99] [99 0] [99 99]}]
    (puzzle cycles wid keep-on input)))