(ns advent-of-code.2023.day-09-v2
  (:require [advent-of-code.common :as common]))

(defn predict [s]
  (transduce (comp (take-while #(apply not= 0 %)) (map last))
             +
             (iterate #(map - (next %) %) s)))

(comment
  (def lines (common/split-long-lines (slurp "input/2023/09-seqs.txt")))
  (def lines (common/split-long-lines "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45\n"))

  ;; year 2023 day 09 puzzle 1
  (transduce (map predict) + lines)
  ;; => 1834108701

  ;; year 2023 day 09 puzzle 2
  (transduce (comp (map reverse) (map predict)) + lines)
  ;; => 993
  )
