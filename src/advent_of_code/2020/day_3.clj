(ns advent-of-code.2020.day-3
  (:require [clojure.string :as str]))

;; --- Day 3: Toboggan Trajectory ---

(defn find-trees [[r d] lines]
  (->> (take-nth d lines)
       (keep-indexed (fn [n line] (#{\#} (nth line (* n r)))))
       count))
  
(defn puzzle [slopes input]
  (let [input (map cycle (str/split-lines input))]
    (reduce * (map #(find-trees % input) slopes))))

(comment
  (puzzle [[3 1]] (slurp "input/2020/3-trees.txt"))

  (puzzle [[1 1] [3 1] [5 1] [7 1] [1 2]] (slurp "input/2020/3-trees.txt"))

  (let [input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"]
    [(puzzle [[3 1]] input)
     (puzzle [[1 1] [3 1] [5 1] [7 1] [1 2]] input)]))
