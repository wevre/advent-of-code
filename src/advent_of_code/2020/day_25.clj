(ns advent-of-code.2020.day-25
  (:require [clojure.edn :as edn]))

(defn txf [subj val] (mod (* subj val) 20201227))

(defn public-loop-size [subj val]
  (count (take-while #(not= % val) (iterate (partial txf subj) 1))))

(defn puzzle1 [input]
  (let [public-keys (edn/read-string (str "[" input "]"))
        loop-size (map #(public-loop-size 7 %) public-keys)]
    (first (drop (first loop-size) (iterate (partial txf (second public-keys)) 1)))))

(comment
  (puzzle1 "5764801 17807724")
  
  (puzzle1 (slurp "input/2020/25-keys.txt")))
