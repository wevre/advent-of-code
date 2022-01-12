(ns advent-of-code.2016.day-08
  (:require [clojure.string :as str]))

;;;; --- Day 8: Two-Factor Authentication ---
;;;; https://adventofcode.com/2016/day/8

(def extents [6 50])

(defn parse-input [s]
  (for [line (str/split-lines s)
        :let [[op a b] (re-seq #"rect|row|col|\d+" line)]]
    [(keyword op) (parse-long a) (parse-long b)]))

(defn rotate [screen axis i v]
  (let [wid (nth extents (- 1 axis))
        rot (fn [val add div] (mod (+ val add) div))]
    (->> screen
         (map (fn [loc]
                (if (= i (nth loc axis))
                  (update loc (- 1 axis) rot v wid)
                  loc)))
         (into #{}))))

(defn update-screen [screen [op a b]]
  (case op
    :rect (into screen (for [r (range b) c (range a)] [r c]))
    :row (rotate screen 0 a b)
    :col (rotate screen 1 a b)))

(defn print-screen [screen]
  (doseq [r (range (nth extents 0))]
    (doseq [c (range (nth extents 1))]
      (print (if (screen [r c]) \# \.)))
    (println))
  (println))

(comment
  (with-redefs [extents [3 7]]
    (let [input (parse-input "rect 3x2
rotate column x=1 by 1
rotate row y=0 by 4
rotate column x=1 by 1")]
      (doseq [s (reductions update-screen #{} input)]
        (print-screen s)))))

(comment
  ;; part 1
  (let [input (parse-input (slurp "input/2016/08-screen.txt"))]
    (count (reduce update-screen #{} input)))   ;=> 116

  ;; part 2
  (let [input (parse-input (slurp "input/2016/08-screen.txt"))]
    (print-screen (reduce update-screen #{} input)))   ;=> UPOJFLBCEZ
  )
