(ns advent-of-code.2015.day-7-v3
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 7: Some Assembly Required ---

(def op<-sym {'AND    bit-and
              'OR     bit-or
              'LSHIFT bit-shift-left
              'RSHIFT unsigned-bit-shift-right})

(defn parse [env s]
  (let [[a b c d e] (edn/read-string (str "(" s ")"))]
    (cond
      (= b '->) (assoc env c a)
      (= a 'NOT) (assoc env d (list bit-not b))
      :else (assoc env e (list (op<-sym b) a c)))))

(def evaluate
  (memoize
   (fn [env sym]
     (let [v (get env sym sym)]
       (cond
         (integer? v) v
         (symbol? v) (evaluate env v)
         :else (apply (first v) (map (partial evaluate env) (rest v))))))))

(defn puzzle1 [sym input]
  (let [env (reduce parse {} (str/split-lines input))]
    (bit-and 0xffff (evaluate env sym))))

(defn puzzle2 [sym override input]
  (let [env (reduce parse {} (str/split-lines input))
        ans (bit-and 0xffff (evaluate env sym))]
    (bit-and 0xffff (evaluate (assoc env override ans) sym))))

(comment
  (let [input (slurp "input/2015/7-wires.txt")] (puzzle1 'a input))
  
  (let [input (slurp "input/2015/7-wires.txt")] (puzzle2 'a 'b input))
  
  (let [input "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"]
    (puzzle1 'g input)))
