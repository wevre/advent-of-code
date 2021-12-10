(ns advent-of-code.2021.day-5
  (:require [clojure.string :as str]))

;; --- Day 5: Hydrothermal Venture ---

(def test-file "input/2021/5-lines-test.txt")
(def file "input/2021/5-lines.txt")

(defn parse [file]
  (->> (slurp file) (re-seq #"\d+") (map #(Integer/parseInt %)) (partition 4)))

(comment
  (parse test-file))

(defn is-ortho? [[x0 y0 x1 y1]]
  (or (= x0 x1) (= y0 y1)))

(comment
  (->> (parse test-file) (map is-ortho?)))

(defn get-points [[x0 y0 x1 y1]]
  (let [len (max (Math/abs (- x1 x0)) (Math/abs (- y1 y0)))
        x-step (cond (< x0 x1) +1 (> x0 x1) -1 :else 0)
        y-step (cond (< y0 y1) +1 (> y0 y1) -1 :else 0)]
    (for [i (range (inc len))]
      [(+ x0 (* i x-step)) (+ y0 (* i y-step))])))

(defn puzzle [file filt]
  (->> (parse file) (filter filt) (mapcat get-points)
       (reduce (fn [acc val] (update acc val (fnil inc 0))) {})
       (reduce-kv (fn [acc k v] (if (< 1 v) (conj acc k) acc)) [])
       count))

(comment
  (puzzle test-file is-ortho?)
  (puzzle file is-ortho?)
  
  (puzzle test-file (constantly true))
  (puzzle file (constantly true)))
