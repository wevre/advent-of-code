(ns advent-of-code.2015.day-14
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 14: Reindeer Olympics ---

(defn distance [seconds [speed fly rest]]
  (let [[q r] ((juxt quot rem) seconds (+ fly rest))]
    (* speed (+ (* fly q) (min r fly)))))

(defn puzzle1 [seconds input]
  (->> (str/split-lines input)
       (map #(map edn/read-string (re-seq #"\d+" %)))
       (map (partial distance seconds))
       (reduce max)))

(comment
  (let [input (slurp "input/2015/14-reindeer.txt")] (puzzle1 2503 input))
  
  (let [input "Comet flies 14 km/s for 10 seconds, rests for 127 
Dancer flies 16 km/s for 11 seconds, rests for 162"]
    (puzzle1 1000 input)))

(defn winners [seconds reindeer]
  (let [flown (group-by #(distance seconds %) reindeer)]
    (get flown (apply max (keys flown)))))

(defn puzzle2 [seconds input]
  (let [reindeer (->> (str/split-lines input)
                      (map #(map edn/read-string (re-seq #"\d+" %))))]
    (->> (range 1 (inc seconds))
         (mapcat #(winners % reindeer))
         frequencies
         vals
         (reduce max))))

(comment
  (let [input (slurp "input/2015/14-reindeer.txt")] (puzzle2 2503 input))
  
  (let [input "Comet flies 14 km/s for 10 seconds, rests for 127 
Dancer flies 16 km/s for 11 seconds, rests for 162"]
    (puzzle2 1000 input)))
