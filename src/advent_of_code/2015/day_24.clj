(ns advent-of-code.2015.day-24-v2
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code.common :refer [parse-longs]]))

;;;; --- Day 24: It Hangs in the Balance ---
;;;; https://adventofcode.com/2015/day/24

;;; Based on reading redditor suggestions.

(defn entangled
  "Find smallest subset with lowest entanglement such that remaining ps _also_
   can form a valid subset."
  [targ]
  (fn entangled
    ([ps] (entangled 1 ps))
    ([i ps]
     (if (< (count ps) i)
       nil
       (if-let [r (->> (combo/combinations ps i)
                       (filter #(= targ (reduce + %)))
                       (sort-by #(reduce * %))
                       ;; Ensures remaining partitions are also valid.
                       ;; But can still find solutions without it.
                       #_(filter #(entangled targ (remove (set %) ps)))
                       seq)]
         (first r)
         (recur (inc i) ps))))))

(comment
  ;; part 1
  (let [input (parse-longs (slurp "input/2015/24-packages.txt"))
        targ (/ (reduce + input) 3)]
    (->> ((entangled targ) input) (reduce *)))   ;=> 10723906903

  ;; part 2
  (let [input (parse-longs (slurp "input/2015/24-packages.txt"))
        targ (/ (reduce + input) 4)]
    (->> ((entangled targ) input) (reduce *)))   ;=> 74850409
  )

;;; I love this solution but I gave up waiting for it to complete.

(defn partitions [input]
  (for [p (combo/partitions input :min 3 :max 3)
        :when (apply = (map #(reduce + %) p))]
    (sort p)))

(comment
  (let [input #_[1 2 3 4 5 7 8 9 10 11] (parse-longs (slurp "input/2015/24-packages.txt"))]
    (->> (partitions input)
         (reduce (fn [[a _ _ :as p1] [b _ _ :as p2]]
                   (cond
                     (< (count a) (count b)) p1
                     (> (count a) (count b)) p2
                     :else (if (< (reduce * a) (reduce * b)) p1 p2))))
         first
         (reduce *)))

  (->> (partitions [1 2 3 4 5 7 8 9 10 11])
       (reduce (fn [acc [a _b _c]] (min acc (reduce * a))) ##Inf))
  )
