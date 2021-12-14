(ns advent-of-code.2021.day-3
  (:require [clojure.string :as str]))

;; --- Day 3: Binary Diagnostic ---
;; https://adventofcode.com/2021/day/3

(defn parse-input [s] (->> s str/split-lines (map #(re-seq #"\d" %))))

(defn dec<-bin "input is a coll of string 1's or 0's." [ss]
  (Integer/parseInt (apply str ss) 2))

(defn most-common [freq-map]
  (let [ones (get freq-map "1") zeros (get freq-map "0")]
    (if (<= zeros ones) "1" "0")))

(defn least-common [freq-map]
  (let [ones (get freq-map "1") zeros (get freq-map "0")]
    (if (<= zeros ones) "0" "1")))

(defn power-consumption [input]
  (->> input
       (apply map vector)   ;; column-major order
       (map frequencies)
       (map (juxt most-common least-common))
       (apply map vector)   ;; back to row-major order
       (map dec<-bin)
       (apply *)))

(comment
  ;; puzzle 1
  (power-consumption (parse-input (slurp "input/2021/3-diagnostics.txt"))))

(defn find-rating
  "Find the least/most-common first bit of each list in input, filter out lists
   that don't have that first bit, then recur on the `next` of each list. Stop
   when there is only one list left in the input."
  [commonest]
  (fn [input]
    (loop [bits [] [i & is :as input] input]
      (if (nil? is)
        (concat bits i)   ;; Join this `i` with accumulated commonest bits.
        (let [mask (commonest (frequencies (map first input)))]
          (recur (conj bits mask)   ;; Growing list of commonest bit.
                 (map next (filter #(= mask (first %)) input))))))))

(defn life-support-rating [input]
  (->> input
       ((juxt (find-rating most-common) (find-rating least-common)))
       (map dec<-bin)
       (apply *)))

(comment
  ;; puzzle 2
  (life-support-rating (parse-input (slurp "input/2021/3-diagnostics.txt"))))
