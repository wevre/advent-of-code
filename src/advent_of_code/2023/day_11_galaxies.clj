(ns advent-of-code.2023.day-11-galaxies
  (:require [advent-of-code.common :as common]
            [clojure.math.combinatorics :as combo]))

(defn count-gaps [a b empty's]
  (let [[a b] (sort [a b])]
    (->> empty's (filter #(< a % b)) count)))

(defn path-dist [[ra ca] [rb cb] empty-rows empty-cols factor]
  (+ (abs (- rb ra))
     (abs (- cb ca))
     (* (dec factor) (+ (count-gaps rb ra empty-rows)
                        (count-gaps cb ca empty-cols)))))

(defn parse [input]
  (let [{:keys [locmap size]} (common/locmap<- input)
        galaxies (->> locmap (keep (fn [[loc c]] (when (= c \#) loc))))
        empty's (fn [f] (->> (range (f size))
                             (remove (set (map f galaxies)))
                             sort))]
    {:galaxies galaxies
     :empty-rows (empty's first)
     :empty-cols (empty's second)}))

(defn solve [{:keys [galaxies empty-rows empty-cols]} expand]
  (transduce (map (fn [[a b]] (path-dist a b empty-rows empty-cols expand)))
             +
             (combo/combinations galaxies 2)))

(comment
  (def input (parse (slurp "input/2023/11-galaxies.txt")))
  (def input (parse "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....\n"))

  ;; year 2023 day 11 puzzle 1
  (solve input 2)
  ;; => 9769724

  ;; year 2023 day 11 puzzle 2
  (solve input 1000000)
  ;; => 603020563700
  )
