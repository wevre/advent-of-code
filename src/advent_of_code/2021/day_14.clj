(ns advent-of-code.2021.day-14
  (:require [clojure.string :as str]))

;; --- Day 14: Extended Polymerization ---
;; https://adventofcode.com/2021/day/14

(defn parse-input [s]
  (let [[template rules] (str/split s #"\n\n")
        last-element (last template)
        template (frequencies (partition 2 1 template))
        rules (->> (re-seq #"([A-Z]+) -> ([A-Z])" rules)
                   (map (fn [[_ [a b] [c]]] {[a b] [[a c] [c b]]}))
                   (into {}))]
    {:template template :rules rules :init {[last-element] 1}}))

(defn apply-rules
  "(Return function to) expand polymer with rules into a long list of
   single-entry maps, then `merge-with +` back into a single map."
  [rules init]
  (fn [polymer]
    (->> polymer
         (mapcat (fn [[pair count]] (map #(hash-map % count) (rules pair))))
         (apply merge-with + init))))

(defn insert-pairs [n {:keys [template rules init]}]
  (->> (iterate (apply-rules rules init) template) (drop n) first))

(defn count-elements
  "Return difference of most- and least-common elements."
  [polymer]
  (->> polymer
       (map (fn [[k v]] {(first k) v}))   ; Can't use `update-keys` because
       (apply merge-with +)               ; collisions; could use `reduce-kv`.
       vals
       (apply (juxt max min))
       (apply -)))

(comment
  ;; puzzle 1
  (time
   (->> (slurp "input/2021/14-polymers.txt")
        parse-input
        (insert-pairs 10)
        count-elements))

  ;; puzzle 2
  (time
   (->> (slurp "input/2021/14-polymers.txt")
        parse-input
        (insert-pairs 40)
        count-elements))
  )