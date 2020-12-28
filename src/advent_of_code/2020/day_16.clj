(ns advent-of-code.2020.day-16
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(defn parse-rules
  "Parses rules for a single ticket field, returning a vector of the form
   [<field-name> <set of valid values>]"
  [line]
  (let [[field ranges] (str/split line #":")]
    [field (->> (re-seq #"\d+" ranges)
                (map edn/read-string)
                (partition 2)
                (reduce (fn [s [x y]] (into s (range x (inc y)))) #{}))]))

(defn puzzle1 [in]
  (let [[s1 _s2 s3] (str/split in #"\n\n")
        ranges (map parse-rules (str/split-lines s1))
        all-valid (reduce set/union (map second ranges))
        nearby (map edn/read-string (re-seq #"\d+" s3))]
    (reduce + (remove all-valid nearby))))

(comment
  (puzzle1 (slurp "input/2020/16-tickets.txt")))

(defn puzzle2 [in]
  (let [[s1 s2 s3] (str/split in #"\n\n")
        ticket (mapv edn/read-string (re-seq #"\d+" s2))
        ranges (map parse-rules (str/split-lines s1))
        all-valid (reduce set/union (map second ranges))
        valid-tickets (->> (drop 1 (str/split-lines s3))
                           (map #(edn/read-string (str "[" % "]")))
                           (filter #(every? all-valid %)))
        fields-at-pos (->> (apply map vector valid-tickets)
                           (map #(keep (fn [[fld valid?]]
                                         (when (every? valid? %) fld))
                                       ranges))
                           (map set))]
    ; `fields-at-pos` is a list where the ith element is a set of fields that
    ; match the values at the ith position. One of those will have a single 
    ; field that matches, we move that into final, and remove it from all
    ; the other positions, rinse and repeat.
    (loop [final {} fields-at-pos fields-at-pos]
      (if-let [[pos field] 
               (first (keep-indexed (fn [i item] (when (= 1 (count item))
                                                   [i (first item)]))
                                    fields-at-pos))]
        (recur (assoc final pos field) (map #(disj % field) fields-at-pos))
        (reduce * (keep (fn [[k v]] (when (str/starts-with? v "departure")
                                      (ticket k)))
                        final))))))

(comment
  (puzzle2 (slurp "input/2020/16-tickets.txt")))