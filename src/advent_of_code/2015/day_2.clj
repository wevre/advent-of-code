(ns advent-of-code.2015.day-2
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 2: I Was Told There Would Be No Math ---

(defn parse [line] (sort (map edn/read-string (re-seq #"\d+" line))))

(defn puzzle [calc input]
  (->> (str/split-lines input)
       (map parse)
       (map calc)
       (reduce +)))

(defn calc-wrapper [[x y z]] (+ (* 3 x y) (* 2 x z) (* 2 y z)))

(defn calc-ribbon [[x y z]] (+ x x y y (* x y z)))

(comment
  (let [input (slurp "input/2015/2-dimensions.txt")] 
    (puzzle calc-wrapper input))

  (let [input "2x3x4\n1x1x10"] (puzzle calc-wrapper input))
  
  (let [input (slurp "input/2015/2-dimensions.txt")]
    (puzzle calc-ribbon input))

  (let [input "2x3x4\n1x1x10"] (puzzle calc-wrapper input)))
