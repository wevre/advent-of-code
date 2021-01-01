(ns advent-of-code.2020.day-21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 21: Allergen Assessment ---

(defn parse [line]
  (let [[ingredients allergens] (str/split line #"contains")
        ingredients (set (re-seq #"\w+" ingredients))
        allergens (re-seq #"\w+" allergens)]
    (zipmap allergens (repeat ingredients))))

(defn puzzle1 [input]
  (let [foods (map parse (str/split-lines input))
        allergens (reduce (partial merge-with set/intersection) foods)
        ingredients (frequencies (mapcat #(val (first %)) foods))]
    (->> (vals allergens)
         (reduce (partial apply dissoc) ingredients)
         vals
         (apply +))))

(defn puzzle2 [input]
  (let [foods (map parse (str/split-lines input))
        allergens (reduce (partial merge-with set/intersection) foods)]
    (->>
     (loop [final () allergens allergens]
       (if-let [[aller ingred :as item]
                (->> allergens
                     (keep (fn [[k v]] (when (= 1 (count v)) [k (first v)])))
                     first)]
         (recur (conj final item) 
                (reduce-kv (fn [acc k v] (assoc acc k (disj v ingred))) {} 
                           allergens))
         final))
     sort
     (map second)
     (str/join ","))))

(comment
  (puzzle1 (slurp "input/2020/21-allergens.txt"))
  
  (puzzle2 (slurp "input/2020/21-allergens.txt"))
  
  (puzzle2 "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")
  )
