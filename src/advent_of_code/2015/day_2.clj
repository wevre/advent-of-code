(ns advent-of-code.2015.day-2
  (:require [clojure.string :as str]))

(defn parse [line]
  (->> (str/split line #"x")
       (map #(Integer/parseInt %))))

(defn calc-wrapper [[x y z]]
  (+ (* 3 x y) (* 2 x z) (* 2 y z)))

(defn puzzle1 [in]
  (->> in
       str/split-lines
       (map parse)
       (map sort)
       (map calc-wrapper)
       (reduce +)))

(comment
  (let [input "2x3x4
1x1x10"] (puzzle1 input))
  
  (let [input (slurp "input/2015/2-dimensions.txt")] (puzzle1 input)))

(defn calc-ribbon [[x y z]]
  (+ x x y y (* x y z)))

(defn puzzle2 [in]
  (->> in
       str/split-lines
       (map parse)
       (map sort)
       (map calc-ribbon)
       (reduce +)))

(comment
  (let [input "2x3x4
1x1x10"] (puzzle2 input))
  
  (let [input (slurp "input/2015/2-dimensions.txt")] (puzzle2 input)))
