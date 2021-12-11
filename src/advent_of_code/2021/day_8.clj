(ns advent-of-code.2021.day-8
  (:require [clojure.set :as set]
            [clojure.pprint :refer [pprint]]))

;; --- Day 8: Seven Segment Search ---

(comment
  ;; puzzle1: find how many "easy" numbers are in the output of each entry
  (->> (slurp "input/2021/8-digits.txt")
       (re-seq #"[a-g]+")
       (partition 14)        ;; divide into entries
       (map #(drop 10 %))    ;; keep only the "output" portion
       (mapcat #(map count %))
       (filter #{2 4 3 7})   ;; keep only the "easy" number of segments
       count)                ;;=> 1000
  )

(def easy "map from (unique) number of segments to known digit"
  {2 1, 4 4, 3 7, 7 8})

(defn resolve-easy [freq-map known]
  (reduce (fn [acc [cnt dig]]
            (let [segs (first (freq-map cnt))]
              (assoc acc dig segs)))
          known
          easy))

(defn resolve-six-segs [freq-map known]
  (let [four (nth known 4)
        one (nth known 1)
        six-segs (freq-map 6)]
    (reduce (fn [m v]
              (cond
                (set/subset? four v) (assoc m 9 v)
                (set/subset? one v) (assoc m 0 v)
                :else (assoc m 6 v)))
            known
            six-segs)))

(defn resolve-five-segs [freq-map known]
  (let [one (nth known 1)
        nine (nth known 9)
        five-segs (freq-map 5)]
    (reduce (fn [m v]
              (cond
                (set/subset? one v) (assoc m 3 v)
                (set/subset? v nine) (assoc m 5 v)
                :else (assoc m 2 v)))
            known
            five-segs)))

(defn num<-digits [digits]
  (->> digits
       (map str)
       (apply str)
       Integer/parseInt))

(comment
  (num<-digits '(1 2 3 4))   ;;=> 1234
  )

(defn resolve-entry [entry]
  (let [[input output] (split-at 10 entry)
        freq-map (group-by count input)
        known (->> (into [] (repeat 10 nil))
                   (resolve-easy freq-map)
                   (resolve-six-segs freq-map)
                   (resolve-five-segs freq-map))
        lookup (reduce-kv (fn [m k v] (assoc m v k)) {} known)]
    (->> output
         (map lookup)
         num<-digits)))

(defn puzzle2 [file]
  (->> (slurp file)
       (re-seq #"[a-g]+")
       (map set)
       (partition 14)
       (map resolve-entry)
       (apply +)))

(comment
  (puzzle2 "input/2021/8-digits.txt")
  )

;; zelark had a different approach: there is a unique combination of the
;; frequency of each of the segments used for the easy numbers and the others.
;; For example, segment 'a' is used by 2 of the easy's and 6 of the other
;; numbers, and so on for each of the other segments. So if you do frequencies
;; of all the segments, split by easy and other, that will give you a unique
;; mapping from random segment, back to correct segment.
