(ns advent-of-code.2020.day-3
  (:require [clojure.string :as str]))

(defn find-trees [lines [r d]]
  (->> (take-nth d lines)
       (map-indexed (fn [n line] (#{\#} (nth line (* n r)))))
       (filter some?)
       count))
  
(defn puzzle [in slopes]
  (let [in (map cycle (str/split-lines in))]
    (reduce * (map (partial find-trees in) slopes))))

(comment
  (puzzle (slurp "input/2020/3-trees.txt") [[3 1]])

  (puzzle (slurp "input/2020/3-trees.txt") [[1 1] [3 1] [5 1] [7 1] [1 2]])

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
    [(puzzle input [[3 1]])
     (puzzle input [[1 1] [3 1] [5 1] [7 1] [1 2]])]))
