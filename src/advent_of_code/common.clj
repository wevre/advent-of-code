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
  (->> (re-seq #"\d+" s)
       (map parse-long)))
