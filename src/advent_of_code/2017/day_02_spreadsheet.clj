(ns advent-of-code.2017.day-02-spreadsheet
  (:require [advent-of-code.common :refer [parse-longs]]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn parse-input [s]
  (for [line (str/split-lines s)] (parse-longs line)))

(defn checksum [num's]
  (->> num's
       (apply (juxt max min))
       (apply -)))

(defn ?evenly-divisible [[a b]]
  ;; NOTE: already sorted
  (and (not= a b) (= 0 (rem a b))))

(defn find-divisibles [num's]
  (->> (combo/combinations (sort > num's) 2)
       (filter ?evenly-divisible)
       (take 1)
       first))

(comment

  (find-divisibles [2 5 7 9 10])

  (do
    (def input (slurp "input/2017/02-spreadsheet.txt"))
    (def nums (parse-input input)))

  ;; year 2017 day 02 puzzle 1
  (->> nums
       (map checksum)
       (reduce +))

  ;; year 2017 day 02 puzzle 2
  (->> nums
       (map find-divisibles)
       (map #(apply / %))
       (reduce +))
  )
