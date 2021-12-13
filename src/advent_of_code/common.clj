(ns advent-of-code.common
  (:require [clojure.string :as str]))

(defn locmap<-digits [s]
  (->> s
       (str/split-lines)
       (map-indexed vector)
       (mapcat (fn [[r l]]
                 (map-indexed (fn [c v] [[r c] (Character/digit v 10)]) l)))
       (into {})))

(defn parse-longs [s]
  (->> (re-seq #"\d+" s)
       (map parse-long)))
