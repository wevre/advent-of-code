(ns advent-of-code.2022.day-13-distress-packets
  (:require [advent-of-code.common :as common]
            [clojure.edn :as edn]))

;; 2022-12-12 10:46
;;    Initial, crude solutions.


(defn packet-compare
  ([a b] (packet-compare 20 a b))
  ([n a b]
   (if (<= n 0)
     0
     (cond
       (and (nil? a) (nil? b)) 0
       (nil? a) -1
       (nil? b) +1
       (and (number? a) (number? b)) (compare a b)
       (and (vector? a) (vector? b)) (let [test (packet-compare (dec n) (first a) (first b))]
                                       (if (zero? test)
                                         (recur (- n 2) (vec (rest a)) (vec (rest b)))
                                         test))
       (number? a) (recur (dec n) [a] b)
       :else (recur (dec n) a [b])))))

(defn right-order? [[s1 s2]]
  (->> (map edn/read-string [s1 s2])
       (apply packet-compare 20 )
       ))


(comment
  (let [input (->> (slurp "input/2022/13-distress-packets.txt")
                   common/split-grouped-lines
                   (map right-order?)
                   (map vector (drop 1 (range)))
                   (keep (fn [[a b]] (when (< b 0) a)))
                   (apply +)
                   )]
    input)

  (let [input (->> (slurp "input/2022/13-distress-packets.txt")
                   common/split-grouped-lines
                   (apply concat)
                   (map edn/read-string)
                   (concat [[[2]] [[6]]])
                   (sort packet-compare)
                   (map vector (drop 1 (range)))
                   (keep (fn [[a b]] (when (#{[[2]] [[6]]} b) a)))
                   (apply *))]
    input)

  (flatten [[2 [3]]])
  (seqable? [] #_(first [1 2 3])))
