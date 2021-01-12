(ns advent-of-code.2015.day-6
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 6: Probably a Fire Hazard ---

(defn parse [s]
  {:cmd (re-find #"on|off|toggle" s) 
   :coords (map edn/read-string (re-seq #"\d+" s))})

(comment
  (parse "turn on 0,0 through 999,999")
  (parse "toggle 0,0 through 999,0")
  (parse "turn off 499,499 through 500,500"))

(defn update-light [grid [action loc]]
  (case action
    "on"     (assoc! grid loc true)
    "off"    (assoc! grid loc false)
    "toggle" (assoc! grid loc (not (grid loc)))))

(defn update-grid [updater grid {cmd :cmd [x0 y0 x1 y1] :coords}]
  (reduce updater 
          grid 
          (for [x (range x0 (inc x1)) y (range y0 (inc y1))] [cmd [x y]])))

(defn puzzle [update-f input]
  (->> (str/split-lines input)
       (map parse)
       (reduce (partial update-grid update-f) (transient {}))
       persistent!
       vals))

(defn puzzle1 [input]
  (count (filter true? (puzzle update-light input))))

(comment
  (let [input (slurp "input/2015/6-lights.txt")] (puzzle1 input))
  
  (puzzle1 "turn on 0,0 through 999,999")
  (puzzle1 "toggle 0,0 through 999,0")
  (puzzle1 "turn off 499,499 through 500,500"))

(defn update-brightness [grid [action loc]]
  (case action
    "on"     (assoc! grid loc (+ (get grid loc 0) 1))
    "off"    (assoc! grid loc (max 0 (- (get grid loc 0) 1)))
    "toggle" (assoc! grid loc (+ (get grid loc 0) 2))))

(defn puzzle2 [input]
  (reduce + (puzzle update-brightness input)))

(comment
  (let [input (slurp "input/2015/6-lights.txt")] (puzzle2 input))

  (let [input "turn on 0,0 through 0,0
toggle 0,0 through 999,999"]
    (puzzle2 input)))
