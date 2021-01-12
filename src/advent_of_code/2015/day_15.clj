(ns advent-of-code.2015.day-15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn score [ingredients weights]
  (->> (map (fn [ps w] (map #(* % w) ps)) ingredients weights)
       (apply map +)
       (map #(if (pos? %) % 0))
       butlast
       (reduce *)))

(defn calories [ingredients weights]
  (reduce + (map (fn [ps w] (* (last ps) w)) ingredients weights)))

(comment
  (calories '((-1 -2 6 3 8) (2 3 -2 -1 3)) '(40 60)))

(defn puzzle1 [tsps input]
  (let [ingreds (map #(map edn/read-string (re-seq #"-?\d+" %))
                     (str/split-lines input))
        parts (for [i1 (range (inc tsps))
                    i2 (range (inc (- tsps i1)))
                    i3 (range (inc (- tsps i1 i2)))
                    :let [i4 (- tsps i1 i2 i3)]]
                [i1 i2 i3 i4])]
    (reduce max (map #(score ingreds %) parts))))

(comment
  (let [input (slurp "input/2015/15-ingredients.txt")] (puzzle1 100 input))

  (let [input "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"] (puzzle1 100 input)))

(defn puzzle2 [tsps input]
  (let [ingreds (map #(map edn/read-string (re-seq #"-?\d+" %))
                     (str/split-lines input))
        parts (for [i1 (range (inc tsps))
                    i2 (range (inc (- tsps i1)))
                    i3 (range (inc (- tsps i1 i2)))
                    :let [i4 (- tsps i1 i2 i3)]
                    :when (= 500 (calories ingreds [i1 i2 i3 i4]))]
                [i1 i2 i3 i4])]
    (reduce max (map #(score ingreds %) parts))))

(comment
  (let [input (slurp "input/2015/15-ingredients.txt")] (puzzle2 100 input))
  
  (let [input "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"] (puzzle2 100 input)))
