(ns advent-of-code.2022.day-25-snafu-nums
  (:require [clojure.string :as str]))

(def snafu {\0 0 \1 1 \2 2 \= -2 \- -1})
(def ufans {0 \0 1 \1 2 \2 3 \= 4 \-})

(defn decimal<- [n]
  (reduce (fn [s d] (+ (* 5 s) (snafu d))) 0 n))

(defn snafu<- [n]
  (loop [acc () n n]
    (if-not (pos? n)
      (apply str acc)
      (let [r (rem n 5)
            carry (if (<= 3 r 4) 1 0)]
        (recur (conj acc (ufans r)) (+ (/ (- n r) 5) carry))))))

(comment
  ;; puzzle 1
  (->> (slurp "input/2022/25-snafu-numbers.txt")
       str/split-lines
       (map decimal<-)
       (apply +)
       snafu<-)
  )
