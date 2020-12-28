(ns advent-of-code.2020.day-5
  (:require [clojure.string :as str]))

(defn convert [in]
  (->> (str/split-lines (str/escape in {\B \1 \F \0 \R \1 \L \0}))
       (map #(Integer/parseInt % 2))
       sort))

(defn puzzle1 [in]
  (last (convert in)))

(defn puzzle2 [in]
  (reduce #(if (= (inc %1) %2) %2 (reduced (inc %1))) (convert in)))

(comment
  
  (puzzle1 "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL")
  
  (puzzle1 (slurp "input/2020/5-boarding_passes.txt"))
  
  (puzzle2 (slurp "input/2020/5-boarding_passes.txt")))
