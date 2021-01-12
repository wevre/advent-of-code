(ns advent-of-code.2015.day-1)

;; --- Day 1: Not Quite Lisp ---

(defn puzzle1 [input] (reduce + (map {\( +1 \) -1} input)))

(comment
  (let [input (slurp "input/2015/1-floors.txt")] (puzzle1 input)))

(defn puzzle2 [input]
  (->> (map {\( +1 \) -1} input)
       (reductions + 0)   ; Need init val of 0 to get proper count.
       (reduce (fn [r v] (if (neg? v) (reduced r) (inc r))) 0)))

(comment
  (let [input (slurp "input/2015/1-floors.txt")] (puzzle2 input)))
