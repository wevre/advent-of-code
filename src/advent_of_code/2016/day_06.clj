(ns advent-of-code.2016.day-06
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map-by]]))

;;;; --- Day 6: Signals and Noise ---
;;;; https://adventofcode.com/2016/day/6

(defn parse-input [comparator s]
  (->> (for [column (apply map vector (str/split-lines s))]
         (into (priority-map-by comparator) (frequencies column)))
       (map ffirst)
       (apply str)))

(comment
  ;; part 1
  (parse-input > (slurp "input/2016/06-message.txt"))   ;=> "qzedlxso"

  ;; part 2
  (parse-input < (slurp "input/2016/06-message.txt"))   ;=> "ucmifjae"
  )
