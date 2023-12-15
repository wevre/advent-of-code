(ns advent-of-code.2023.day-11-v2
  (:require [advent-of-code.common2 :as common2]
            [clojure.math.combinatorics :as combo]))

(defn dist [a b coords expand]
  (transduce (map (fn [x] (if (coords x) 1 expand)))
             +
             (range (min a b) (max a b))))

(defn solve [galaxies expand]
  (let [row-coords (set (map first galaxies))
        col-coords (set (map second galaxies))]
    (transduce (map (fn [[[ar ac] [br bc]]]
                      (+ (dist ar br row-coords expand)
                         (dist ac bc col-coords expand))))
               +
               (combo/combinations galaxies 2))))

(defn parse
  "Create a locmap then return a set of the galaxy (#) loc's."
  [input]
  (keys (into {} (common2/locmap<- #{\#}) input)))

(comment
  (def galaxies (parse (slurp "input/2023/11-galaxies.txt")))
  (def galaxies (parse "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....\n"))

  ;; year 2023 day 11 puzzle 1
  (solve galaxies 2)
  ;; => 9769724

  ;; year 2023 day 11 puzzle 2
  (solve galaxies 1000000)
  ;; => 603020563700
  )
