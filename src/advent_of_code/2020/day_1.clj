(ns advent-of-code.2020.day-1
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;; A brute-corce approach, trying every possible combination. There are only 
;; about 200 numbers in the input file, so this works okay. Another approach 
;; that works for the 2-number sum, is to sort the numbers, then search from
;; each end until we find the combination that matches the goal.

(defn combo-sum [xs sum cnt]
  (->> (combo/combinations xs cnt)
       (filter #(= sum (apply + %)))
       first))

(defn puzzle [in sum cnt]
  (let [xs (->> in
                str/split-lines
                (map #(Integer/parseInt %)))]
    (apply * (combo-sum xs sum cnt))))

(comment
  (let [input "1721\n979\n366\n299\n675\n1456"]
    [(puzzle input 2020 2)
     (puzzle input 2020 3)])

  (puzzle (slurp "input/2020/1-expense_report.txt") 2020 2)

  (puzzle (slurp "input/2020/1-expense_report.txt") 2020 3))
