(ns advent-of-code.2020.day-17
  (:require [clojure.string :as str]))

(defn parse [z w lines]
  (reduce-kv (fn [acc y l]
               (into acc (keep-indexed (fn [x c] (when (= c \#) [x y z w])) l)))
             #{}
             (vec lines)))

(def neighbors
  (memoize 
   (fn neighbors [w-rng [x y z w]]
     (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1] dw w-rng :when (not= 0 dx dy dz dw)]
       [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))))

; `neighbors` generates a list of all the neighbors of all active cells, repeats
; intentionally included. Taking `frequencies` of that list produces a map that 
; for each cell answers the question 'How many active neighbors do you have'?

(defn step [neighbors cubes]
  (set (for [[loc n] (frequencies (mapcat neighbors cubes))
             :when (or (= 3 n) (and (cubes loc) (= 2 n)))]
         loc)))

(defn puzzle [n w-rng in]
  (->> (parse 0 0 (str/split-lines in))
       (iterate (partial step (partial neighbors w-rng)))
       (drop n)
       first
       count))

(comment
  (puzzle 2 [0] ".#.\n..#\n###")
  (puzzle 6 [0] (slurp "input/2020/17-cubes.txt"))
  (puzzle 6 [-1 0 1] (slurp "input/2020/17-cubes.txt"))
  )