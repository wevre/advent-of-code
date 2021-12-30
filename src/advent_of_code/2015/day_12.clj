(ns advent-of-code.2015.day-12
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :as walk]))

;;;; --- Day 12: JSAbacusFramework.io ---
;;;; https://adventofcode.com/2015/day/12

(defn parse-input [s]
  (edn/read-string (str/escape s {\: \ })))

(defn solve [input de-map]
  (->> input
       (walk/postwalk #(if (map? %) (de-map %) %))
       flatten
       (filter number?)
       (reduce +)))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2015/12-json.txt"))]
    (solve input #(seq %)))   ;=> 111754

  ;; part 2
  (let [input (parse-input (slurp "input/2015/12-json.txt"))]
    (solve input #(when (not-any? #{"red"} (vals %)) (seq %))))   ;=> 65402
  )
