(ns advent-of-code.2015.day-04
  (:require [clj-commons.digest :refer [md5]]
            [clojure.string :as str]))

;;;; --- Day 4: The Ideal Stocking Stuffer ---
;;;; https://adventofcode.com/2015/day/4

(defn solve [secret prefix]
  (->> (iterate (fn [[i _]] [(inc i) (md5 (str secret (inc i)))]) [0 ""])
       (filter (fn [[_ hash]] (str/starts-with? hash prefix)))
       ffirst))

(def input "ckczppom")

(comment
  ;; part 1 -- 200ms
  (time
   (solve input "00000"))   ;=> 117946

  ;; part 2 -- ~6s
  (time
   (solve input "000000"))   ;=> 3938038

  (solve "abcdef" "00000")
)
