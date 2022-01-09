(ns advent-of-code.2016.day-11
  (:require [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(def init {:E 1
           :F {1 #{[:C :Ru] [:C :Tm] [:C :Co] [:G :Co] [:G :Pm] [:G :Po] [:G :Ru] [:G :Tm]}
               2 #{[:C :Pm] [:C :Po]}
               3 #{}
               4 #{}}})

(defn floor-safe? [floor]
  (let [chips (into #{} (keep (fn [[a b]] (when (= :C a) b)) floor))
        generators (into #{} (keep (fn [[a b]] (when (= :G a) b)) floor))]
    (or (empty? (set/difference chips generators))
        (empty? (set/difference generators chips)))))

(comment
  (mapcat #(combo/combinations [1 2 3 4] %) [1 2])

  (floor-safe? #{[:C :Ru] [:C :Tm] [:C :Co] [:G :Co] [:G :Pm] [:G :Po] [:G :Ru] [:G :Tm]})
  (floor-safe? [ [:G :Pm]])
  )

(defn moves [{i :E floors :F}]
  (for [items (mapcat #(combo/combinations (vec (floors i)) %) [1 2])
        :when (floor-safe? items)
        :let [src (set/difference (floors i) items)]
        :when (floor-safe? src)
        j [(dec i) (inc i)]
        :when (<= 1 j 4)
        :let [dest (set/union (floors j) items)]
        :when (floor-safe? dest)]
    {:E j :F (assoc floors i src j dest)}))

(comment
  (pprint
   (->> (iterate moves init)
        (drop 1)
        first))
  )
