(ns advent-of-code.2021.day-5
  (:require [clojure.string :as str]))

;; --- Day 5: Hydrothermal Venture ---

(def test-file "input/2021/5-lines-test.txt")
(def file "input/2021/5-lines.txt")

(defn parse [file]
  (->> (slurp file) (re-seq #"\d+") (map #(Integer/parseInt %)) (partition 4)))

(comment
  (parse test-file))

(defn is-ortho? [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(comment
  (->> (parse test-file) (map is-ortho?)))

;; Idea for `rangex` came from zelark, with my own modification to handle ortho
;; and diagonal lines all within same function.
(defn rangex [start end]
  (cond
    (< start end) (range start (inc end))
    (< end start) (range start (dec end) -1)
    :else (repeat start)))

(defn get-points [[x1 y1 x2 y2]]
  (map vector (rangex x1 x2) (rangex y1 y2)))

(defn puzzle [file filt]
  (->> (parse file)
       (filter filt)
       (mapcat get-points)         ;; Get all points into one big list.
       (group-by identity)         ;; Group same points.
       vals                        ;; We don't need the keys.
       (filter #(< 1 (count %)))   ;; Keep points hit by more than one line.
       count))

(comment
  ;; For part one we keep only orthogonal lines.
  (puzzle test-file is-ortho?)
  (puzzle file is-ortho?)

  ;; For part two we let all lines through.
  (puzzle test-file (constantly true))
  (puzzle file (constantly true)))
