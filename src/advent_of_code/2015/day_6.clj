(ns advent-of-code.2015.day-6
  (:require [clojure.string :as str]))

(defn parse [s]
  (let [re #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)"
        [_ instr x0 y0 x1 y1] (re-find re s)]
     {:action (keyword (str/replace instr " " "-"))
      :x0 (Integer/parseInt x0)
      :y0 (Integer/parseInt y0)
      :x1 (Integer/parseInt x1)
      :y1 (Integer/parseInt y1)}))

(comment
  (parse "turn on 0,0 through 999,999")
  (parse "toggle 0,0 through 999,0")
  (parse "turn off 499,499 through 500,500"))

(defn update-light [grid [action loc]]
  (case action
    :turn-on  (assoc! grid loc true)
    :turn-off (assoc! grid loc false)
    :toggle   (assoc! grid loc (not (grid loc)))))

(defn update-grid [updater grid {:keys [action x0 y0 x1 y1]}]
  (reduce updater 
          grid 
          (for [x (range x0 (inc x1)) y (range y0 (inc y1))]
            [action [x y]])))

(defn puzzle1 [in]
  (->> in
       (str/split-lines)
       (map parse)
       (reduce (partial update-grid update-light) (transient {}))
       persistent!
       vals
       (filter true?)
       count))

(comment
  (puzzle1 "turn on 0,0 through 999,999")
  (puzzle1 "toggle 0,0 through 999,0")
  (puzzle1 "turn off 499,499 through 500,500")
  
  (let [input (slurp "input/2015/6-lights.txt")]
    (puzzle1 input)))

(defn update-brightness [grid [action loc]]
  (case action
    :turn-on  (assoc! grid loc (+ (get grid loc 0) 1))
    :turn-off (assoc! grid loc (max 0 (- (get grid loc 0) 1)))
    :toggle   (assoc! grid loc (+ (get grid loc 0) 2))))

(defn puzzle2 [in]
  (->> in
       (str/split-lines)
       (map parse)
       (reduce (partial update-grid update-brightness) (transient {}))
       persistent!
       vals
       (reduce +)))

(comment
  (let [input "turn on 0,0 through 0,0
toggle 0,0 through 999,999"] 
    (puzzle2 input))
  
  (let [input (slurp "input/2015/6-lights.txt")]
    (puzzle2 input)))
