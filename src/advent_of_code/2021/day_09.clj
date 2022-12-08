(ns advent-of-code.2021.day-09
  (:require [advent-of-code.common :refer [locmap<-digits]]))

;; --- Day 9: Smoke Basin ---
;; https://adventofcode.com/2021/day/9

(def input (:locmap (locmap<-digits (slurp "input/2021/9-heights.txt"))))

(defn neighbors [[r c]]
  (for [[dr dc] [[0 -1] [0 1] [-1 0] [1 0]]]
    [(+ r dr) (+ c dc)]))

(comment
  (map #(Integer/parseInt %) (re-seq #"\d" "12345"))
  (map #(Character/digit % 10) "12345")
  )

(defn low-point? [hmap [loc height]]
  (->> (neighbors loc)
       (keep hmap)
       (every? #(< height %))))

(defn find-low-points [hmap]
  (filter (partial low-point? hmap) hmap))

(defn puzzle1 []
  (->> input
       find-low-points
       vals
       (map inc)
       (apply +)))

(comment
  (puzzle1))

(defn find-high-adj [hmap loc]
  (let [h (hmap loc)]
    (->> (neighbors loc)
         (select-keys hmap)
         (filter #(< h (val %) 9))
         keys)))

(defn find-basin
  "Return all adjacent locs that flow down into low point `loc`."
  [hmap loc]
  (loop [visited #{} search [loc]]
    (if-let [next-locs (->> (mapcat (partial find-high-adj hmap) search) (remove visited) seq)]
      (recur (into visited search) next-locs)
      (into visited search))))

(comment
  (find-high-adj input [1 2])
  (count (find-basin input [2 3])))

(defn puzzle2 []
  (->> input
       find-low-points   ;; a list (filtered from a map) of [loc height]
       keys              ;; now just a list of `loc`
       (map (partial find-basin input))  ;; list of basin-adjacent locs
       (map count)
       (sort >)
       (take 3)
       (reduce *)))

(comment
  (puzzle2))