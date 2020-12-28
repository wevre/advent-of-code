(ns advent-of-code.2020.day-1
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

; A brute-corce approach, trying every possible combination. There are only 
; about 200 numbers in the input file, so this works okay.

(defn combo-sum [sum cnt xs]
  (->> (combo/combinations xs cnt)
       (filter #(= sum (apply + %)))
       first))

(defn puzzle [sum cnt input]
  (->> (str/split-lines input)
       (map #(Integer/parseInt %))
       (combo-sum sum cnt)
       (apply *)))

(comment
  (puzzle 2020 2 (slurp "input/2020/1-expense_report.txt"))

  (puzzle 2020 3 (slurp "input/2020/1-expense_report.txt"))

  (let [input "1721\n979\n366\n299\n675\n1456"]
    [(puzzle 2020 2 input) (puzzle 2020 3 input)]))
