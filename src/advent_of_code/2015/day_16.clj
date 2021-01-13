(ns advent-of-code.2015.day-16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 16: Aunt Sue ---

(defn filter-aunts [aunts [op x k]]
  (filter #(or (not (% k)) ((resolve op) x (% k))) aunts))

(defn puzzle [rules input]
  (let [aunts (map #(edn/read-string (str "{" (str/escape % {\: \ }) "}"))
                   (str/split-lines input))]
    ('Sue (first (reduce filter-aunts aunts rules)))))

(comment
  (let [input (slurp "input/2015/16-aunts.txt")
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
    (puzzle rules input))
  
  (let [input (slurp "input/2015/16-aunts.txt")
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
    (puzzle rules input)))
