(ns advent-of-code.2020.day-22
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 22: Crab Combat ---

(defn queue [coll] (into clojure.lang.PersistentQueue/EMPTY coll))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map #(str/split % #":"))
       (map second)
       (map #(mapv edn/read-string (re-seq #"\d+" %)))
       (map queue)))

(defn score [deck] (reduce + (map * deck (range (count deck) 0 -1))))

(defn combat [p1 p2]
  (cond
    (empty? p1) [:p2 p2]
    (empty? p2) [:p1 p1]
    :else 
    (let [[c1 r1] ((juxt peek pop) p1)
          [c2 r2] ((juxt peek pop) p2)]
      (if (< c1 c2)
        (recur r1 (conj r2 c2 c1))
        (recur (conj r1 c1 c2) r2)))))

(defn recursive-combat [p1 p2]
  (loop [p1 p1 p2 p2 history #{}]
    (cond
      (history [p1 p2]) [:p1 p1]
      (empty? p1) [:p2 p2]
      (empty? p2) [:p1 p1]
      :else
      (let [history (conj history [p1 p2])
            [c1 r1] ((juxt peek pop) p1)
            [c2 r2] ((juxt peek pop) p2)
            winner (cond
                     (and (<= c1 (count r1)) (<= c2 (count r2)))
                     (first (recursive-combat (queue (take c1 r1))
                                              (queue (take c2 r2))))
                     (> c1 c2) :p1 
                     :else :p2)]
        (if (= :p1 winner)
          (recur (conj r1 c1 c2) r2 history)
          (recur r1 (conj r2 c2 c1) history))))))

(defn puzzle [play input]
  (score (second (apply play (parse input)))))

;; NOTE: use a queue instead of list/vector: clojure.lang.PersistentQueue/EMPTY
;; create a function to (queue coll)
;; Also create a score function instead of repeating that code twice

(comment
  (puzzle recursive-combat (slurp "input/2020/22-cards.txt"))
  
  (puzzle recursive-combat "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"))