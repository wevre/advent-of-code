(ns advent-of-code.2020.day-15
  (:require [clojure.edn :as edn]))

;; --- Day 15: Rambunctious Recitation ---

; An alternate approach would be to split out a function to update one step of 
; the result map, and then iterate n times.

(defn puzzle 
  "Loops over a map that keeps track of numbers spoken and their prior two 
   positions. New numbers get an initial position list of (i i) so 'number of 
   turns apart' since previously spoken will come back as 0."
  [n input]
  (let [parse (map edn/read-string (re-seq #"\d+" input))
        seed (map-indexed #(vector %2 (list %1 %1)) parse)]
    (:res (loop [acc (into {:res (last parse)} seed) i (count seed)]
            (if (< i n)
              (let [x (reduce - (acc (:res acc) '(0 0)))
                    acc (-> (assoc acc :res x)
                            (assoc x (take 2 (cons i (acc x (list i))))))]
                (recur acc (inc i)))
              acc)))))

(comment
  (puzzle 2020 "8,0,17,4,1,12")
  (puzzle 30000000 "8,0,17,4,1,12")

  (let [input '("0,3,6" "1,3,2" "2,1,3" "1,2,3" "2,3,1" "3,2,1" "3,1,2")]
    [(map #(puzzle 2020 %) input) (map #(puzzle 30000000 %) input)]))
