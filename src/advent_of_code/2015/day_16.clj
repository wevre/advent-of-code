(ns advent-of-code.2015.day-16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;;;; --- Day 16: Aunt Sue ---
;;;; https://adventofcode.com/2015/day/16

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(edn/read-string (str "{" (str/escape % {\: \ }) "}")))))

(defn filter-aunts [aunts [op x k]]
  (filter #(or (not (% k)) ((resolve op) x (% k))) aunts))

(comment
  ;; part 1
  (let [aunts (parse-input (slurp "input/2015/16-aunts.txt"))
        rules '((= 3 children)
                (= 7 cats)
                (= 2 samoyeds)
                (= 3 pomeranians)
                (= 0 akitas)
                (= 0 vizslas)
                (= 5 goldfish)
                (= 3 trees)
                (= 2 cars)
                (= 1 perfumes))]
    ('Sue (first (reduce filter-aunts aunts rules))))   ;=> 40

  (let [aunts (parse-input (slurp "input/2015/16-aunts.txt"))
        rules '((= 3 children)
                (< 7 cats)
                (= 2 samoyeds)
                (> 3 pomeranians)
                (= 0 akitas)
                (= 0 vizslas)
                (> 5 goldfish)
                (< 3 trees)
                (= 2 cars)
                (= 1 perfumes))]
    ('Sue (first (reduce filter-aunts aunts rules))))   ;=> 241
  )
