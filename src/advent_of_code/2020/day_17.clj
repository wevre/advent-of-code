(ns advent-of-code.2020.day-17
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;; --- Day 17: Conway Cubes ---

(defn parse [dims [y line]]
  (let [pad (fn [& cs] (take dims (concat cs (repeat 0))))]
    (keep-indexed (fn [x c] (when (= c \#) (pad x y))) line)))

(defn neighbors
  "Generates a list of all neighbors of `cells`, repeats intentionally included.
   Taking `frequencies` of that list produces a map that answers the question 
   'How many neighbors do you have from among `cells`?'"
  [cells]
  (->> (combo/selections [-1 0 1] (count cells))
       (remove #(apply = 0 %))
       (map #(map + cells %))))

(defn step [cubes]
  (set (for [[loc n] (frequencies (mapcat neighbors cubes))
             :when (or (= 3 n) (and (cubes loc) (= 2 n)))]
         loc)))

(defn puzzle [cycles dims input]
  (->> (str/split-lines input)
       (into #{} (comp (map-indexed vector) (mapcat (partial parse dims))))
       (iterate step)
       (drop cycles)
       first
       count))

(comment
  (puzzle 2 3 ".#.\n..#\n###")
  (puzzle 6 3 (slurp "input/2020/17-cubes.txt"))
  (puzzle 6 4 (slurp "input/2020/17-cubes.txt")))
