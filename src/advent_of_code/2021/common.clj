(ns advent-of-code.2021.common
  (:require [clojure.string :as str]))

(defn locmap<-digits [s]
  (->> s
       (str/split-lines)
       (map-indexed vector)
       (mapcat (fn [[r l]]
                 (map-indexed (fn [c v] [[r c] (Character/digit v 10)]) l)))
       (into {})))
