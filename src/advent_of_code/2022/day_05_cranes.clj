(ns advent-of-code.2022.day-05-cranes
  (:require [advent-of-code.common :as common]))

(defn parse-drawing
  "Return a map of stack num to (seq of) crates."
  [lines]
  (->> lines
       (map #(transduce (comp (drop 1) (take-nth 4)) conj %))
       drop-last
       (common/transpose \space)
       (map reverse)
       (map #(remove #{\space} %))
       (map vector (drop 1 (range)))
       (into {})))

(defn parse
  "Parses stack drawing then applies moves, using op to simulate
   CrateMover 9000 (reverse) or CrateMover 9001 (identity)."
  [op input]
  (let [[stacks moves] (common/split-grouped-lines input)
        stacks (parse-drawing stacks)
        moves (map common/parse-longs moves)]
    (reduce (fn [stacks [n from to]]
              (-> stacks
                  (update to concat (op (take-last n (get stacks from))))
                  (update from (partial drop-last n))))
            stacks
            moves)))

(defn topmost [ss]
  (transduce (comp (map second) (map last)) str (sort ss)))

(comment
  ;; puzzle 1
  (topmost (parse reverse (slurp "input/2022/05-cranes.txt")))   ; => SHMSDGZVC

  ;; puzzle 2
  (topmost (parse identity (slurp "input/2022/05-cranes.txt")))   ; => VRZGHDFBQ
  )
