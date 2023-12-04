(ns advent-of-code.2023.day-04-v2
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn card-info [n]
  (fn [line]
    (let [[_card & numbers] (re-seq #"\d+" line)
          [winning have] (split-at n numbers)]
      (count (keep (set winning) have)))))

(defn counts-rf
  ([xs] (reduce + xs))
  ([xs x] (conj xs (reduce + 1 (take x xs)))))

(defn parse [n input]
  (map (card-info n) (str/split-lines input)))

(defn points [wins] (long (math/pow 2 (dec wins))))

(comment
  (def cards (parse 5 (slurp "input/2023/04-sample-scratchcards.txt")))
  (def cards (parse 10 (slurp "input/2023/04-scratchcards.txt")))

  ;; year 2023 day 04 puzzle 1
  (transduce (comp (filter pos?) (map points)) + cards)   ;; => 15205

  ;; year 2023 day 04 puzzle 2
  (time
   (transduce identity counts-rf () (reverse cards)))   ;; => 6189740
  )