(ns advent-of-code.2015.day-23
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-input [s]
  (vec
   (for [line (str/split-lines s)]
     (edn/read-string (str "(:" line ")")))))

(defn execute [program registers]
  (loop [ptr 0 registers registers]
    (let [[op c d] (nth program ptr '(hlt))]
      (case op
        :hlf (recur (inc ptr) (update registers c quot 2))
        :tpl (recur (inc ptr) (update registers c * 3))
        :inc (recur (inc ptr) (update registers c inc))
        :jmp (recur (+ ptr c) registers)
        :jie (recur (+ ptr (if (even? (registers c)) d 1)) registers)
        :jio (recur (+ ptr (if (= 1 (registers c)) d 1)) registers)
        registers))))

(comment
  ;; part 1
  (let [program (parse-input (slurp "input/2015/23-instructions.txt"))]
    ('b (execute program '{a 0 b 0})))   ;=> 170

  ;; part 2
  (let [program (parse-input (slurp "input/2015/23-instructions.txt"))]
    ('b (execute program '{a 1 b 0})))   ;=> 247
  )
