(ns advent-of-code.2022.day-03-rucksacks
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn priority [i]
  (let [code (int i)]
    (if (<= (int \a) code (int \z))
      (- code (int \a) -1)
      (- code (int \A) -27))))

(defn find-common [groups]
  (->> groups
       (map set)
       (apply set/intersection)
       first))

(defn puzzle [input group-fn]
  (->> input
       str/split-lines
       group-fn
       (map find-common)
       (map priority)
       (apply +)))

(comment
  ;; puzzle 1
  (puzzle (slurp "input/2022/03-rucksacks.txt")
          (partial map (fn [s] (split-at (/ (count s) 2) s))))   ; => 8123

  ;; puzzle 2
  (puzzle (slurp "input/2022/03-rucksacks.txt")
          (partial partition 3))   ; => 2620
  )
