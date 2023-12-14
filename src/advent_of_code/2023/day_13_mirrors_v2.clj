(ns advent-of-code.2023.day-13-mirrors-v2
  (:require [advent-of-code.common :as common]))

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
        (* 100 (solve-rows (map seq rows) eps)))))

(defn parse [input]
  (common/split-grouped-lines input))

(comment
  (def input (parse (slurp "input/2023/13-sample.txt")))
  (def input (parse (slurp "input/2023/13-mirrors.txt")))

  ;; year 2023 day 13 puzzle 1
  (reduce + (map (solve 0) input))
  ;; => 34202

  ;; year 2023 day 13 puzzle 2
  (reduce + (map (solve 1) input))
  ;; => 34230
  )
