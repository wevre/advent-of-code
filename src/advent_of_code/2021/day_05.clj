(ns advent-of-code.2021.day-05
  (:require [advent-of-code.common :refer [parse-longs]]))

;; --- Day 5: Hydrothermal Venture ---
;; https://adventofcode.com/2021/day/5

(defn parse-input [s]
  (->> (parse-longs s) (partition 4)))
(defn ortho? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn rangex
  "Return range from start to end, works with diagonal lines.
   Adapted from zelark."
  [start end]
  (cond
    (< start end) (range start (inc end))
    (< end start) (range start (dec end) -1)
    :else (repeat start)))

(defn get-points [[x1 y1 x2 y2]]
  (map vector (rangex x1 x2) (rangex y1 y2)))

(defn overlaps
  ([] (overlaps (constantly true)))
  ([filter-fn]
   (fn [input]
     (->> input
          (filter filter-fn)
          (mapcat get-points)         ;; Get all points into one big list.
          (group-by identity)         ;; Group same points.
          vals                        ;; We don't need the keys.
          (filter #(< 1 (count %)))   ;; Keep points hit by more than one line.
          count))))

(comment
  ;; puzzle 1
  ((overlaps ortho?) (parse-input (slurp "input/2021/5-lines.txt")))

  ;; puzzle 2
  ((overlaps) (parse-input (slurp "input/2021/5-lines.txt")))
 )
