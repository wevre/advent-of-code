(ns advent-of-code.2020.day-18
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]))

;; --- Day 18: Operation Order ---

; First version didn't do postwalk elimination of lists. Without that, the eval 
; functions have to wrap every arg1 and arg2 to test if they are lists, and 
; recursively evaluate them. With postwalk, however, lists are taken care of 
; before eval ever gets called, resulting in cleaner logic.

(defn math-eval [[arg1 op & [arg2 & rest]]]
  (if op (recur (cons ((resolve op) arg1 arg2) rest)) arg1))

(defn adv-eval [[arg1 op & [arg2 & rest]]]
  (case op
    + (recur (cons (+ arg1 arg2) rest))
    * (* arg1 (adv-eval (cons arg2 rest)))
    arg1))

(defn calc [eval line]
  (walk/postwalk #(if (list? %) (eval %) %) line))

(defn puzzle [evaluator input]
  (->> (str/split-lines input)
       (map #(edn/read-string (str "(" % ")")))
       (map (partial calc evaluator))
       (reduce +)))

(comment
  (puzzle math-eval (slurp "input/2020/18-math.txt"))
  (puzzle adv-eval (slurp "input/2020/18-math.txt"))

  (map (juxt (partial calc math-eval) (partial calc adv-eval))
       '((1 + (2 * 3) + (4 * (5 + 6)))
         (2 * 3 + (4 * 5))
         (5 + (8 * 3 + 9 + 3 * 4 * 3))
         (5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)))
         (((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2))))
