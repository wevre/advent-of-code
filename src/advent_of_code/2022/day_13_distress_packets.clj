(ns advent-of-code.2022.day-13-distress-packets
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; 2022-12-12 10:46
;;    Initial, crude solutions.
;; 2022-12-12 11:54
;;    Still not convinced that my comparator is as simple as it can be, but I
;;    did improve parsing somewhat. It is a legit comparator and just like the
;;    docs say, those are tricky to get right.
;; 2022-12-13 00:48
;;    Don't need to map-index followed by keep, just use keep-indexed. Also, my
;;    comparison of the `first` of a and b followed by comparison of `rest` of a
;;    and b can be accomplished with `map`ping packet-compare over a and b and
;;    searching the resulting sequence for first non-zero value. Saw that in a
;;    few others solutions. Also remembered that I used that approach in a
;;    natural sort comparator that I wrote long ago
;;    (https://github.com/wevre/natural-compare).

(defn packet-compare [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) (recur [a] b)
    (number? b) (recur a [b])
    :else
    (or (->> (map packet-compare a b) (drop-while zero?) first)
        (- (count a) (count b)))))

(defn parse [input]
  (->> input str/split-lines (keep edn/read-string)))

(def dividers #{[[2]] [[6]]})

(comment
  ;; puzzle 1
  (->> (parse (slurp "input/2022/13-distress-packets.txt"))
       (partition 2)
       (map #(apply packet-compare %))
       (keep-indexed (fn [i c] (when (< c 0) (inc i))))
       (apply +))   ; => 5905

  ;; puzzle 2
  (->> (parse (slurp "input/2022/13-distress-packets.txt"))
       (concat dividers)
       (sort packet-compare)
       (keep-indexed (fn [i p] (when (dividers p) (inc i))))
       (apply *))   ; => 21691
  )
