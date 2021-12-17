(ns advent-of-code.2021.day-08
  (:require [clojure.set :as set]))

;; --- Day 8: Seven Segment Search ---
;; https://adventofcode.com/2021/day/8

(defn parse-input [s]
  (->> (re-seq #"[a-g]+" s)
       (partition 14)
       (map (partial split-at 10))
       (map #(zipmap [:uniques :output] %))))

(def digit<-segments
  "Map from segments to digit."
  (->> ["abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"]
       (reduce-kv (fn [m k v] (assoc m (set (seq v)) k)) {})))

(defn fingerprint
  "Convert unique patterns (strings) to map of segment to fingerprint."
  [uniques]
  (as-> uniques $
    (map #(zipmap (seq %) (repeat (list (count %)))) $)
    (apply merge-with concat $)
    (update-vals $ sort)))

(defn decode [{:keys [uniques output]}]
  (let [seg<-fprint (set/map-invert (fingerprint (keys digit<-segments)))
        digit-map (update-vals (fingerprint uniques) seg<-fprint)]
    (->> output
         (sequence (comp (map #(map digit-map %))
                         (map #(set %))
                         (map digit<-segments))))))

(comment
  ;; puzzle 1
  (->> (parse-input (slurp "input/2021/8-digits.txt"))
       (sequence (comp (mapcat decode) (keep #{1 4 7 8})))
       count)

  ;; puzzle 2
  (->> (parse-input (slurp "input/2021/8-digits.txt"))
       (transduce (comp (map decode) (map #(apply str %)) (map parse-long)) +))
  )
