(ns advent-of-code.2022.day-13-distress-packets
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; 2022-12-12 10:46
;;    Initial, crude solutions.
;; 2022-12-12 11:54
;;    Still not convinced that my comparator is as simple as it can be, but I
;;    did improve parsing somewhat. It is a legit comparator and just like the
;;    docs say, those are tricky to get right.

(defn packet-compare [a b]
  (cond
    (and (nil? a) (nil? b)) 0
    (nil? a) -1
    (nil? b) +1
    (and (number? a) (number? b)) (compare a b)

    (and (seqable? a) (seqable? b))
    (let [[a1 & ar] a [b1 & br] b test (packet-compare a1 b1)]
      (or (when-not (and a1 b1 (zero? test)) test) (recur ar br)))

    (number? a) (recur [a] b)
    :else (recur a [b])))

(defn parse [input]
  (->> input str/split-lines (keep edn/read-string)))

(def dividers #{[[2]] [[6]]})

(comment
  ;; puzzle 1
  (->> (parse (slurp "input/2022/13-distress-packets.txt"))
       (partition 2)
       (map #(apply packet-compare %))
       (map vector (drop 1 (range)))
       (keep (fn [[a b]] (when (< b 0) a)))
       (apply +))   ; => 5905

  ;; puzzle 2
  (->> (parse (slurp "input/2022/13-distress-packets.txt"))
       (concat dividers)
       (sort packet-compare)
       (map vector (drop 1 (range)))
       (keep (fn [[a b]] (when (dividers b) a)))
       (apply *))   ; => 21691
  )
