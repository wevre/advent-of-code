(ns advent-of-code.2015.day-06
  (:require [clojure.string :as str]))

;;;; --- Day 6: Probably a Fire Hazard ---
;;;; https://adventofcode.com/2015/day/6

(defn parse-input [s]
  (for [line (str/split-lines s)]
    {:cmd (keyword (re-find #"on|off|toggle" line))
     :coords (map parse-long (re-seq #"\d+" line))}))

(def update-light
  {:on (fn [grid loc] (assoc! grid loc 1))
   :off (fn [grid loc] (assoc! grid loc 0))
   :toggle (fn [grid loc] (assoc! grid loc (- 1 (get grid loc 0))))})

(defn rangex [start end] (range start (inc end)))

(defn update-grid [f]
  (fn [grid {cmd :cmd [x0 y0 x1 y1] :coords}]
    (reduce (f cmd) grid (for [x (rangex x0 x1) y (rangex y0 y1)] [x y]))))

(defn solve [input update-f]
  (->> input
       (reduce (update-grid update-f) (transient {}))
       persistent!
       vals
       (reduce +)))

(comment
  ;; part 1 -- 15s
  (time
   (solve (parse-input (slurp "input/2015/6-lights.txt")) update-light))   ;=> 569999
  )

(def update-brightness
  {:on (fn [grid loc] (assoc! grid loc (+ 1 (get grid loc 0))))
   :off (fn [grid loc] (assoc! grid loc (max 0 (- (get grid loc 0) 1))))
   :toggle (fn [grid loc] (assoc! grid loc (+ 2 (get grid loc 0))))})

(comment
  ;; part 2 -- 20s
  (time
   (solve (parse-input (slurp "input/2015/6-lights.txt")) update-brightness))   ;=> 17836115
  )
