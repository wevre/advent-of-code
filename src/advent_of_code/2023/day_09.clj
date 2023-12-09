(ns advent-of-code.2023.day-09
  (:require [clojure.string :as str]
            [advent-of-code.common :as common]))

(defn pairwise-diffs [s]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 s)))

(defn predict [s]
  (if (every? zero? s)
    0
    (+ (last s) (predict (pairwise-diffs s)))))

(defn parse [input] (mapv common/parse-longs (str/split-lines input)))

(comment
  (def lines (parse (slurp "input/2023/09-seqs.txt")))
  (def lines (parse "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45\n"))

  ;; year 2023 day 09 puzzle 1
  (transduce (map predict) + lines)
  ;; => 1834108701

  ;; year 2023 day 09 puzzle 2
  (transduce (comp (map reverse) (map predict)) + lines)
  ;; => 993

  )
