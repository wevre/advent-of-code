(ns advent-of-code.2020.day-16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(defn puzzle1 [in]
  (let [[s1 _s2 s3] (str/split in #"\n\n")
        ranges (partition 2 (map edn/read-string (re-seq #"\d+" s1)))
        valid (reduce (fn [s [x y]] (into s (range x (inc y)))) #{} ranges)
        nearby (map edn/read-string (re-seq #"\d+" s3))]
    (reduce + (remove valid nearby))))

(comment
  (puzzle1 (slurp "input/2020/16-tickets.txt")))

(defn parse-fields 
  "Parses rules for a single ticket field, returning a vector of the form
   [<field-name> <set of valid numbers>]"
  [line]
  (let [[field ranges] (str/split line #":")
        ranges (partition 2 (map edn/read-string (re-seq #"\d+" ranges)))]
    [field (reduce (fn [s [x y]] (into s (range x (inc y)))) #{} ranges)]))

(defn unwind [m]
  (loop [fin {} pnd m]
    (if-let [[x s] (first (filter #(= 1 (count (second %))) pnd))]
      (recur (conj fin [x (first s)]) 
             (reduce-kv (fn [m k v] (assoc m k (disj v (first s)))) 
                        {} 
                        pnd))
      fin)))

(defn puzzle2 [in]
  (let [[s1 s2 s3] (str/split in #"\n\n")
        ranges (into {} (map parse-fields (str/split-lines s1)))
        valid (reduce set/union (vals ranges))
        nearby (->> (drop 1 (str/split-lines s3))
                    (map #(edn/read-string (str "[" % "]")))
                    (filter #(every? valid %)))
        by-field (apply map vector nearby)
        potents (reduce (fn [acc [k v]] (update acc k #(if % (conj % v) #{v})))
                        {}
                        (for [[i f] (map-indexed vector by-field)
                              [k v] ranges
                              :when (every? v f)]
                          [i k]))
        fields (unwind potents)
        depart-fields (reduce-kv (fn [l k v] (if (str/starts-with? v "departure") (conj l k) l)) [] fields)
        ticket (mapv edn/read-string (re-seq #"\d+" s2))]
    (reduce * (map ticket depart-fields))))


(comment
  (puzzle2 (slurp "input/2020/16-tickets.txt"))
  )