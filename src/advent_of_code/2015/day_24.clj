(ns advent-of-code.2015.day-24
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;; --- Day 24: It Hangs in the Balance ---

(defn entangled 
  "Find smallest subset with lowest entanglement such that remaining ps _also_
   can form a valid subset."
  [targ ps]
  (loop [i 1]
    (cond
      (empty? ps) ()
      (< (count ps) i) nil
      :else 
      (if-let [r (->> (combo/combinations ps i)
                      (filter #(= targ (reduce + %)))
                      (sort-by #(reduce * %))
                      (filter #(entangled targ (remove (set %) ps)))
                      seq)]
        (first r)
        (recur (inc i))))))

(defn puzzle [groups input]
  (let [packages (map edn/read-string (re-seq #"\d+" input))
        targ (/ (apply + packages) groups)
        res (entangled targ packages)]
    [(reduce * res) res]))

(comment
  (let [input "1\n2\n3\n4\n5\n7\n8\n9\n10\n11"]
    [(puzzle 3 input) (puzzle 4 input)])

  (time (let [input (slurp "input/2015/24-packages.txt")]
          [(puzzle 3 input) (puzzle 4 input)]))

  (let [input (str/join " " '(1 2 3 6 7 8 9 11 16))]
    (puzzle 3 input))
  (let [input (str/join " " '(1 3 5 13 17 23 31 37 53))]
    (puzzle 3 input)))
