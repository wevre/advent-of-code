(ns advent-of-code.2020.day-23-2
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 23: Crab Cups ---

(defn next-dest [max-n curr]
  (let [dest (dec curr)]
    (if (< dest 1) max-n dest)))

(defn play [limit max-n cups]
  (loop [i 0 curr (nth cups 0) cups cups]
    (if (< i limit)
      (let [three (take 3 (rest (iterate (partial nth cups) curr)))
            dest (->> (iterate (partial next-dest max-n) curr)
                      rest
                      (drop-while (set three))
                      first)
            cups (-> cups
                     (assoc! curr (nth cups (last three)))
                     (assoc! (last three) (nth cups dest))
                     (assoc! dest (first three)))]
        (recur (inc i) (nth cups curr) cups))
      cups)))

(defn puzzle1 [lim max-n input]
  (let [labels (map edn/read-string (re-seq #"\d" input))
        cups (reduce (fn [acc c] (-> (assoc! acc (nth acc 0) c) (assoc! 0 c)))
                     (transient (vec (repeat 10 0)))
                     (take 10 (cycle labels)))
        cups (play lim max-n cups)]
    (str/join (take 8 (rest (iterate (partial nth cups) 1))))))

(defn puzzle2 [lim max-n input]
  (let [labels (map edn/read-string (re-seq #"\d" input))
        cups (reduce (fn [acc c] (-> (assoc! acc (nth acc 0) c) (assoc! 0 c)))
                     (transient (vec (repeat 10 0)))
                     (take (inc max-n) (cycle (concat labels 
                                                      (range 10 (inc max-n))))))
        cups (play lim max-n cups)]
    (reduce * (take 2 (rest (iterate (partial nth cups) 1))))))

(comment
  ; example input
  (puzzle1 100 9 "389125467")   ;=>67384529
  ; puzzle input
  (puzzle1 100 9 "974618352")   ;=>75893264
  
  ; example input
  (puzzle2 10000000 1000000 "389125467")   ;=>149245887792
  ; puzzle input
  (puzzle2 10000000 1000000 "974618352")   ;=>38162588308
  )