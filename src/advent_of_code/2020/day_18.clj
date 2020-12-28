(ns advent-of-code.2020.day-18
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn math-eval [[op1 op & [op2 & rest]]]
  (if op
    (recur (cons ((resolve op)
                  (if (list? op1) (math-eval op1) op1)
                  (if (list? op2) (math-eval op2) op2))
                 rest))
    (if (list? op1) (recur op1) op1)))

(defn adv-eval [[op1 op & [op2 & rest]]]
  (cond
    (= op '+) (recur (cons ((resolve op)
                            (if (list? op1) (adv-eval op1) op1)
                            (if (list? op2) (adv-eval op2) op2))
                           rest))
    (= op '*) ((resolve op) 
               (if (list? op1) (adv-eval op1) op1) 
               (adv-eval (cons op2 rest)))
    :else (if (list? op1) (recur op1) op1)))

(defn puzzle [evaluator in]
  (->> (str/split-lines in)
       (map #(edn/read-string (str "(" % ")")))
       (map evaluator)
       (reduce +)))

(comment
  (puzzle math-eval (slurp "input/2020/18-math.txt"))
  (puzzle adv-eval (slurp "input/2020/18-math.txt"))
    
  (map (juxt math-eval adv-eval) 
       '((1 + (2 * 3) + (4 * (5 + 6)))
         (2 * 3 + (4 * 5))
         (5 + (8 * 3 + 9 + 3 * 4 * 3))
         (5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)))
         (((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2)))
  )
