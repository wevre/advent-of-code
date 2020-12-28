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
   [<field-name> <set of valid values]"
  [line]
  (let [[field ranges] (str/split line #":")]
    [field (->> (re-seq #"\d+" ranges)
                (map edn/read-string)
                (partition 2)
                (reduce (fn [s [x y]] (into s (range x (inc y)))) #{}))]))

(defn deduce 
  "Input is a map of {<position> <set of valid fields for that position>}.
   Finds a position that only has 1 valid field, adds it to the final map, and
   removes it from all other positions. Rinse and repeat."
  [inp]
  (loop [fin {} inp inp]
    (if-let [[pos fields] (first (filter #(= 1 (count (second %))) inp))]
      (let [field (first fields)]
        (recur (conj fin [pos field]) 
               (reduce-kv (fn [m k v] (assoc m k (disj v field))) {} inp)))
      fin)))

(defn puzzle2 [in]
  (let [[s1 s2 s3] (str/split in #"\n\n")
        ticket (mapv edn/read-string (re-seq #"\d+" s2))
        ranges (into {} (map parse-fields (str/split-lines s1)))
        nearby (->> (drop 1 (str/split-lines s3))
                    (map #(edn/read-string (str "[" % "]")))
                    (filter #(every? (reduce set/union (vals ranges)) %))
                    (apply map vector)
                    (map-indexed vector)
                    (into {}))]
    nearby #_
    ; `nearby` is a list with an entry of `[pos [val1, val2, ...]]` for each
    ; position, and with invalid tickets filtered out.
    (->> (for [[pos vals] nearby [fld valid?] ranges 
               :when (every? valid? vals)] 
           [pos fld])
         (group-by first)
         (reduce-kv (fn [m k v] (assoc m k (set (map second v)))) {})
         deduce
         (filter #(str/starts-with? (second %) "departure"))
         (map first)
         (map ticket)
         (reduce *))))

(comment
  (puzzle2 (slurp "input/2020/16-tickets.txt")))