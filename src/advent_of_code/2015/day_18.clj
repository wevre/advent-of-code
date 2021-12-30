(ns advent-of-code.2015.day-18
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;;;; --- Day 18: Like a GIF For Your Yard ---
;;;; https://adventofcode.com/2015/day/18

(def wid 100)

(defn parse-input [s]
  (for [[r l] (map-indexed vector (str/split-lines s))
        [c v] (map-indexed vector l)
        :when (= v \#)]
    [r c]))

(defn neighbors [cell]
  (->> (combo/selections [-1 0 1] (count cell))
       (remove #(apply = 0 %))
       (map #(map + cell %))))

(defn step [wid keep-on]
 (fn [grid]
   (into keep-on
         (set (for [[[r c :as loc] n] (frequencies (mapcat neighbors grid))
                    :when (or (= 3 n) (and (grid loc) (= 2 n)))
                    :when (and (< -1 r wid) (< -1 c wid))]
                loc)))))

(defn solve [input keep-on steps]
  (->>
   (iterate (step wid keep-on) input)
   (drop steps)
   first
   count))

(comment
  ;; part 1
  (let [keep-on #{}
        input (into keep-on (parse-input (slurp "input/2015/18-lights.txt")))]
    (solve input keep-on 100))   ;=> 814

  ;; part 2
  (let [keep-on #{[0 0] [0 99] [99 0] [99 99]}
        input (into keep-on (parse-input (slurp "input/2015/18-lights.txt")))]
    (solve input keep-on 100))   ;=> 924
  )
