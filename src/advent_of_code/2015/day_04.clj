(ns advent-of-code.2015.day-04
  (:require [clj-commons.digest :refer [md5]]
            [clojure.string :as str]))

;;;; --- Day 4: The Ideal Stocking Stuffer ---
;;;; https://adventofcode.com/2015/day/4

(defn solve [secret prefix]
  (->> (map #(vector % (md5 (str secret %))) (drop 1 (range)))
       (keep (fn [[i hash]] (when (str/starts-with? hash prefix) i)))
       first))

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
