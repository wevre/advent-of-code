(ns advent-of-code.2021.day-24
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :refer [postwalk]]))

;;;; --- Day 24: Arithmetic Logic Unit ---
;;;; https://adventofcode.com/2021/day/24

(def instr-per-digit 18)
(def param-rows #{4 5 15})

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(str "(" % ")"))
       (map edn/read-string)
       (postwalk #(if (symbol? %) (keyword %) %))))

(defn valid-ranges
  "Examine params, find the mult/divide pairs that affect output and return
  valid ranges of digits, as a vector of `[min max]`, for each column."
  [params]
  (loop [[[a b c] & params] params col 0 stack [] res {}]
    (case a
      26 (let [{c-col :col c :c} (peek stack)
               delta (+ c b)   ; `c` from mult column, `b` from divide column.
               res (-> res
                       (assoc c-col [(max 1 (- 1 delta)) (min 9 (- 9 delta))])
                       (assoc col [(max 1 (+ 1 delta)) (min 9 (+ 9 delta))]))]
           (recur params (inc col) (pop stack) res))
      1 (recur params (inc col) (conj stack {:col col :c c}) res)
      res)))

(defn get-params
  "Read in MONAD, extract crucial parameters for each digit."
  [input]
  (->> input
       (map-indexed vector)
       (keep (fn [[i [_op _a b]]] (when (param-rows (mod i instr-per-digit)) b)))
       (partition 3)))

(defn solve
  "Apply function f to each digits valid range."
  [f input]
  (let [ranges (valid-ranges (get-params input))]
    (apply str (for [c (range 14)] (apply f (ranges c))))))

(comment
  ;; part 1
  (solve max (parse-input (slurp "input/2021/24-alu.txt")))

  ;; part 2
  (solve min (parse-input (slurp "input/2021/24-alu.txt")))
  )

;;; Defines ALU that can confirm a model number.

(defn alu [code]
  (fn [input]
    (let [input (->> input seq (map #(inc (Character/digit % 10))))]
      (loop [{input :input :as state} {:w 0 :x 0 :y 0 :z 0 :input input} [[op a b] & code] code]
        (let [b (state b b)]
          (case op
            :inp (recur (assoc state a (first input) :input (rest input)) code)
            :add (recur (update state a + b) code)
            :mul (recur (update state a * b) code)
            :div (recur (update state a quot b) code)
            :mod (recur (update state a mod b) code)
            :eql (recur (update state a #(if (= %1 %2) 1 0) b) code)
            state))))))

(comment

  (time
   (let [input (parse-input (slurp "input/2021/24-alu.txt"))
         monad (alu input)]
     (:z (monad "12996997829399"))))

  (require '[clojure.pprint :refer [pprint]])

  ;; 24-alu-alt.txt input has solutions 74929995999389 and 11118151637112
)