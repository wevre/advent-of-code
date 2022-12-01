(ns advent-of-code.2022.day-01
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map common/parse-longs)
       (map #(reduce + %))
       (sort >)))

(defn puzzle1 [input]
  (->> input parse first))

(comment
  (puzzle1 (slurp "input/2022/01-calories.txt")))

(defn puzzle2 [input]
  (->> input parse (take 3) (reduce +)))

(comment
  (puzzle2 (slurp "input/2022/01-calories.txt")))
