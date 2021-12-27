(ns advent-of-code.2015.day-07
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;;;; --- Day 7: Some Assembly Required ---
;;;; https://adventofcode.com/2015/day/7

(def op<-sym {'AND    bit-and
              'OR     bit-or
              'LSHIFT bit-shift-left
              'RSHIFT unsigned-bit-shift-right})

(defn parse-input [s]
  (loop [[l & ls] (str/split-lines s) env {}]
    (if-not l
      env
      (let [[a b c d e] (edn/read-string (str "(" l ")"))]
        (cond
          (= b '->) (recur ls (assoc env c a))
          (= a 'NOT) (recur ls (assoc env d (list bit-not b)))
          :else (recur ls (assoc env e (list (op<-sym b) a c))))))))

(defn fix [f] (fn g [& args] (apply f g args)))

(defn evaluator [env]
  (fix
   (memoize
     ;; This is our actual evaluate function. In order for memoize to work, this
     ;; function is provided, as its first parameter, the function it should
     ;; call to continue evaluating (which happens to be itself).
    (fn [f sym]
      (let [v (get env sym sym)]
        (cond
          (integer? v) v
          (symbol? v) (f v)
          :else (apply (first v) (map f (rest v)))))))))

(comment
  ;; part 1 -- 6ms
  (time
   (let [env (parse-input (slurp "input/2015/7-wires.txt"))]
     (bit-and 0xffff ((evaluator env) 'a))))   ;=> 3176

  ;; part 2 -- 27ms
  (time
   (let [env (parse-input (slurp "input/2015/7-wires.txt"))
         ans (bit-and 0xffff ((evaluator env) 'a))
         env (assoc env 'b ans)]
     (bit-and 0xffff ((evaluator env) 'a))))   ;=> 14710
  )
