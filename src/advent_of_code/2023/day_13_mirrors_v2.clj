(ns advent-of-code.2023.day-13-mirrors-v2
  (:require [advent-of-code.common2 :as common2]))

;; Want to explore the stack trick from @erdos.

(defn solve-rows [rows eps]
  (loop [up (take 1 rows) dn (drop 1 rows)]
    (when dn
      (if (= eps (reduce + (map #(if (= %1 %2) 0 1) (flatten up) (flatten dn))))
        (count up)
        (recur (conj up (first dn)) (next dn))))))

(defn solve [eps]
  (fn [rows]
    (or (solve-rows (apply map vector rows) eps)
        (* 100 (solve-rows rows eps)))))

(comment
  (def input (sequence (common2/split-grouped-lines) (slurp "input/2023/13-sample.txt")))
  (def input (sequence (common2/split-grouped-lines) (slurp "input/2023/13-mirrors.txt")))

  ;; year 2023 day 13 puzzle 1
  (transduce (map (solve 0)) + input)
  ;; => 34202

  ;; year 2023 day 13 puzzle 2
  (transduce (map (solve 1)) + input)
  ;; => 34230
  )
