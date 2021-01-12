(ns advent-of-code.2015.day-15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 15: Science for Hungry People ---

(defn score [ingredients weights]
  (->> (map (fn [ps w] (map #(* % w) ps)) ingredients weights)
       (apply map +)
       (map #(if (pos? %) % 0))
       butlast
       (reduce *)))

(defn calories [ingredients weights]
  (reduce + (map (fn [ps w] (* (last ps) w)) ingredients weights)))

(defn puzzle [tsps pred input]
  (let [ingreds (map #(map edn/read-string (re-seq #"-?\d+" %))
                     (str/split-lines input))
        parts (for [i1 (range (inc tsps))
                    i2 (range (inc (- tsps i1)))
                    i3 (range (inc (- tsps i1 i2)))
                    :let [i4 (- tsps i1 i2 i3)]]
                [i1 i2 i3 i4])]
    (reduce max (map #(score ingreds %) (filter #(pred ingreds %) parts)))))

(comment
  (let [input (slurp "input/2015/15-ingredients.txt")]
    (puzzle 100 (constantly true) input))

  (let [input (slurp "input/2015/15-ingredients.txt")]
    (puzzle 100 (fn [is w] (= 500 (calories is w))) input)))
