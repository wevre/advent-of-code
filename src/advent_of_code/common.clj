(ns advent-of-code.common
  (:require [clojure.string :as str]))

(defn locmap<-digits
  ([s] (locmap<-digits s identity))
  ([s f]
   (->> s
        (str/split-lines)
        (map-indexed vector)
        (mapcat (fn [[r l]]
                  (map-indexed (fn [c v] [[r c] (f (Character/digit v 10))]) l)))
        (into {}))))

(defn parse-longs [s]
  (map parse-long (re-seq #"-?\d+" s)))

(defn range-inc
  "Return range from `start` to `end`, inclusive. `end` need not be greater than
   `start`. Adapted from zelark (so it works even if start == end)."
  ([end] (range-inc 0 end))
  ([start end]
   (cond
     (< start end) (range start (inc end))
     (< end start) (range start (dec end) -1)
     :else (repeat start))))

(defn points-along [[x1 y1] [x2 y2]]
  (map vector (range-inc x1 x2) (range-inc y1 y2)))
