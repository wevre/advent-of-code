(ns advent-of-code.2016.day-01
  (:require [clojure.java.math :as math]
            [advent-of-code.common :refer [points-along]]))

;;;; --- Day 1: No Time for a Taxicab ---
;;;; https://adventofcode.com/2016/day/1

(defn parse-input [s]
  (for [[d v] (partition 2 (re-seq #"R|L|\d+" s))]
    [d (parse-long v)]))

(def orient {"N" {"L" [- :x "W"], "R" [+ :x "E"]}
             "E" {"L" [+ :y "N"], "R" [- :y "S"]}
             "S" {"L" [+ :x "E"], "R" [- :x "W"]}
             "W" {"L" [- :y "S"], "R" [+ :y "N"]}})

(defn path [dirs]
  (reductions (fn [{dir :dir :as res} [hand v]]
                (let [[f axis new-dir] (get-in orient [dir hand])]
                  (-> res
                      (assoc :dir new-dir)
                      (update axis f v))))
              {:dir "N" :x 0 :y 0}
              dirs))

(defn grid-distance [x y] (+ (math/abs x) (math/abs y)))

(defn trace-points [path]
  (loop [[{:keys [x y]} & ps] (drop 1 path) visited #{} last-point [0 0]]
    (let [loc [x y]
          ls (points-along last-point loc)]
      (if-let [dup (some visited (rest ls))]
        dup
        (recur ps (into visited ls) loc)))))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2016/01-directions.txt"))
        {:keys [x y]} (last (path input))]
    (grid-distance x y))   ;=> 209

  ;; part 2
  (let [input (parse-input (slurp "input/2016/01-directions.txt"))]
    (->> (path input)
         trace-points
         (apply grid-distance)))   ;=> 136
  )