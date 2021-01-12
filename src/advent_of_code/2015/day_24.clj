(ns advent-of-code.2015.day-24-v2
  (:require [clojure.edn :as edn]))

;; --- Day 24: It Hangs in the Balance ---

(defn entangled [targ xs]
  (let [valid-remain? (fn [xs] (or (empty? xs) (seq (entangled targ xs))))
        step
        (fn step [res acc n [f & r]]
          (cond
            (neg? n) res
            (< 0 (count res) (count acc)) res
            (zero? n) (if (and (or (empty? res)
                                   (< (count acc) (count res))
                                   (< (reduce * acc) (reduce * res)))
                               (valid-remain? (remove (set acc) xs)))
                        acc
                        res)
            (nil? f) res
            :else
            (let [res (step res (conj acc f) (- n f) r)]
              (recur res acc n r))))]
    (step () () targ xs)))

(defn puzzle [groups input]
  (let [packages (map edn/read-string (re-seq #"\d+" input))
        targ (/ (apply + packages) groups)
        res (entangled targ (reverse packages))]
    [(reduce * res) res]))

(comment
  (time (let [input (slurp "input/2015/24-packages.txt")]
          [(puzzle 3 input) (puzzle 4 input)]))

  (let [input "1\n2\n3\n4\n5\n7\n8\n9\n10\n11"]
    [(puzzle 3 input) (puzzle 4 input)]))
