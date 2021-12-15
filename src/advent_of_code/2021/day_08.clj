(ns advent-of-code.2021.day-08
  "Idea here is to create a 'fingerprint' for each segment, which is the count
   of segments in each digit where the segment appears. For example, segment 'a'
   is in the digits (0 2 3 5 6 7 8 9), and those digits have segment counts
   of (6 5 5 5 6 3 7 6), respectively. Sorted, those are (3 5 5 5 6 6 6 7),
   which is a unique fingerprint for segment 'a'. The other segments 'b' through
   'g' likewise have unique fingerprints. We fingerprint the randomized input
   and use it to identify the correct segment and ultimately the output digits."
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
