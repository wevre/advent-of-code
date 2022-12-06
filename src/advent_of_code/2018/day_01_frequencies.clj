(ns advent-of-code.2018.day-01-frequencies
  (:require [advent-of-code.common :as common]))

(comment
  ;; puzzle 1
  (->> (slurp "input/2018/01-frequencies.txt")
       common/parse-longs
       (apply +))

  ;; puzzle 2
  (->> (slurp "input/2018/01-frequencies.txt")
       common/parse-longs
       cycle
       (reductions + 0)
       (reduce (fn [acc v] (if (acc v) (reduced v) (conj acc v))) #{}))
  )
