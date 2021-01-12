(ns advent-of-code.2015.day-3
  (:require [clojure.set]))

;; --- Day 3: Perfectly Spherical Houses in a Vacuum ---

(defn visited [input]
  (->> input
       (map {\^ [0 1] \> [1 0] \< [-1 0] \v [0 -1] \- [0 0]})
       (reductions (partial map +) [0 0])
       set))

(defn puzzle1 [input]
  (count (visited input)))

(comment
  (let [input (slurp "input/2015/3-houses.txt")] (puzzle1 input))
  
  (let [input ">"] (puzzle1 input))
  (let [input "^>v<"] (puzzle1 input))
  (let [input "^v^v^v^v^v"] (puzzle1 input)))

(defn puzzle2 [input]
  (->> (partition 2 input (str input "-"))   ; add "-" to ensure even
       (apply map vector)
       (mapcat visited)
       set
       count))

(comment
  (let [input (slurp "input/2015/3-houses.txt")] (puzzle2 input))
  
  (let [input "^v"] (puzzle2 input))
  (let [input "^>v<"] (puzzle2 input))
  (let [input "^v^v^v^v^v"] (puzzle2 input)))
