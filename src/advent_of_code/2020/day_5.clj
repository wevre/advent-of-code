(ns advent-of-code.2020.day-5
  (:require [clojure.string :as str]))

;; --- Day 5: Binary Boarding ---

(defn convert [in]
  (->> (str/split-lines (str/escape in {\B \1 \F \0 \R \1 \L \0}))
       (map #(Integer/parseInt % 2))))

(defn puzzle1 [input]
  (last (sort (convert input))))

(defn puzzle2 
  "Finds gap between seat locations"
  [input]
  (reduce #(if (= (inc %1) %2) %2 (reduced (inc %1))) (sort (convert input))))

(comment
  
  (puzzle1 "BFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL")
  
  (puzzle1 (slurp "input/2020/5-boarding_passes.txt"))
  
  (puzzle2 (slurp "input/2020/5-boarding_passes.txt")))
