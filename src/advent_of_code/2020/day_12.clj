(ns advent-of-code.2020.day-12
  (:require [clojure.string :as str]))

(defn parse [line]
  (let [[_ action value] (re-find #"(.)(\d+)" line)]
    {:action (keyword action) :value (Integer/parseInt value)}))

(defn navigate [ship {:keys [action value]}]
  (case action
    :N (update ship :y + value)
    :E (update ship :x + value)
    :S (update ship :y - value)
    :W (update ship :x - value)
    :F (case (mod (:rot ship) 360)
         0 (update ship :x + value)
         90 (update ship :y + value)
         180 (update ship :x - value)
         270 (update ship :y - value))
    :L (update ship :rot + value)
    :R (update ship :rot - value)))

(defn manhattan-distance [ship]
  (+ (Math/abs (:x ship)) (Math/abs (:y ship))))

(defn puzzle1 [in]
  (->> (str/split-lines in)
       (map parse)
       (reduce navigate {:x 0 :y 0 :rot 0})
       manhattan-distance))

(comment
  (let [input "F10
N3
F7
R90
F11"] (puzzle1 input))
  
  (let [input (slurp "input/2020/12-directions.txt")] (puzzle1 input)))

(defn rotate-waypoint [{:keys [w-x w-y] :as ship} val]
  (apply assoc ship 
         (case (mod val 360)
           0 [:w-x w-x :w-y w-y]
           90 [:w-x w-y :w-y (- w-x)]
           180 [:w-x (- w-x) :w-y (- w-y)]
           270 [:w-x (- w-y) :w-y w-x])))

(defn waypoint [ship {:keys [action value]}]
  (case action
    :N (update ship :w-y + value)
    :E (update ship :w-x + value)
    :S (update ship :w-y - value)
    :W (update ship :w-x - value)
    :F (-> ship
           (update :x + (* value (:w-x ship)))
           (update :y + (* value (:w-y ship))))
    :L (rotate-waypoint ship (- value))
    :R (rotate-waypoint ship value)))

(defn puzzle2 [in]
  (->> (str/split-lines in)
       (map parse)
       (reduce waypoint {:x 0 :y 0 :w-x 10 :w-y 1})
       manhattan-distance))

(comment
  (let [input "F10
N3
F7
R90
F11"] (puzzle2 input))

  (let [input (slurp "input/2020/12-directions.txt")] (puzzle2 input)))